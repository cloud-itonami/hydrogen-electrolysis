(ns hydrogen-electrolysis.methods.test-reactor-closure
  "Tests for the getter-loading + hermetic-closure decision contract
  (hydrogen reactor fabrication cell, activities preceding the leak gate)."
  (:require [clojure.test :refer [deftest is]]
            [hydrogen-electrolysis.methods.reactor-closure :as rc]))

;; ---------------------------------------------------------------------------
;; Shared fixtures
;; ---------------------------------------------------------------------------

(def good-loading-request
  {:reactor/serial "RX-2026-0007"
   :request/effects #{:simulate-getter-loading-plan}
   :approvals #{:load-getter}
   :getter/material "SAES St707 / Zr-V-Fe"
   :getter/lot "GET-LOT-0042"
   :getter/data-sheet-url "https://supplier.example/datasheets/st707.pdf"
   :design-record/activation-temp-c 400
   :design-record/getter-mass-g 12.5
   :procedure/atmosphere-limits {:o2-ppm-max 10
                                 :h2o-ppm-max 20}
   :measured/atmosphere {:o2-ppm 3
                         :h2o-ppm 8}
   :interlocks-present #{:dry-inert-glovebox-verified
                         :o2-h2o-monitor-verified
                         :no-ignition-sources
                         :class-d-extinguisher
                         :sealed-transfer-container-ready}})

(def good-closure-request
  {:reactor/serial "RX-2026-0007"
   :request/effects #{:simulate-closure-plan}
   :approvals #{:perform-closure}
   :closure/joint-method :laser-welding
   :closure/wps-record-id "WPS-H2RX-001"
   :design-record/closure-temp-c 1200
   :interlocks-present #{:hot-work-permit-current
                         :fire-watch-assigned
                         :no-hydrogen-service-in-area
                         :fume-extraction-verified
                         :class-d-extinguisher
                         :machine-guard-verified}})

(def good-offer
  {:offer/name "Example Vacuum Brazing Furnace VB-90"
   :source-url "https://example-furnace.example/products/vb-90"
   :condition :refurbished
   :price 184000
   :currency "USD"})

;; ---------------------------------------------------------------------------
;; Activity 1 — plan getter loading
;; ---------------------------------------------------------------------------

(deftest test-loading-happy-path-approves-simulate-only
  (let [d (rc/plan-getter-loading good-loading-request)]
    (is (= :decision/approved (:decision d)))
    (is (= :effect/simulate-getter-loading-plan (:effect d)))
    (is (= "RX-2026-0007" (get-in d [:plan :reactor/serial])))
    (is (= "GET-LOT-0042" (get-in d [:plan :getter/lot])))
    (is (= :unmeasured-by-this-contract (get-in d [:plan :getter/capacity])))
    (is (= :deferred-to-reactor-fab-leak-gate
           (get-in d [:plan :post-loading-leak-gate])))
    (is (= #{:load-getter} (:human-approvals-used d)))
    (is (= :hydrogen-reactor-fabrication (:audit/cell d)))
    (is (false? (:audit/bot-commanded-equipment d)))
    (is (false? (:audit/values-invented d)))))

(deftest test-loading-bot-command-refused-even-with-approval
  (let [d (rc/plan-getter-loading
           (assoc good-loading-request :request/effects #{:command-equipment}))]
    (is (= :decision/refused (:decision d)))
    (is (= :effect/refuse (:effect d)))
    (is (= [:command-equipment] (:audit/refused-effects d)))))

(deftest test-loading-activate-getter-live-refused
  (let [d (rc/plan-getter-loading
           (assoc good-loading-request :request/effects #{:activate-getter-live}))]
    (is (= :decision/refused (:decision d)))))

(deftest test-loading-no-getter-in-design-refused-not-filled-in
  (let [d (rc/plan-getter-loading
           (assoc good-loading-request :getter/material nil))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/no-getter-in-design-record (:reason d)))))

(deftest test-loading-missing-serial-refused
  (let [d (rc/plan-getter-loading
           (assoc good-loading-request :reactor/serial ""))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/missing-traceability (:reason d)))))

(deftest test-loading-missing-lot-refused
  (let [d (rc/plan-getter-loading
           (assoc good-loading-request :getter/lot nil))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/missing-traceability (:reason d)))))

(deftest test-loading-missing-data-sheet-refused
  (let [d (rc/plan-getter-loading
           (assoc good-loading-request :getter/data-sheet-url "http://not-https.example"))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/missing-first-party-data-sheet (:reason d)))))

(deftest test-loading-unmeasured-inputs-listed-never-invented
  (let [d (rc/plan-getter-loading
           (assoc good-loading-request
                  :design-record/activation-temp-c nil
                  :measured/atmosphere nil))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/unmeasured-getter-loading-inputs (:reason d)))
    (is (= [:design-record/activation-temp-c :measured/atmosphere]
           (:unmeasured d)))))

(deftest test-loading-zero-mass-is-unmeasured-not-zero
  (let [d (rc/plan-getter-loading
           (assoc good-loading-request :design-record/getter-mass-g 0))]
    (is (= :decision/blocked (:decision d)))
    (is (= [:design-record/getter-mass-g] (:unmeasured d)))))

(deftest test-loading-atmosphere-above-limits-blocked
  (let [d (rc/plan-getter-loading
           (assoc-in good-loading-request [:measured/atmosphere :o2-ppm] 25))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/atmosphere-out-of-limits (:reason d)))
    (is (= #{:o2} (:violated d)))))

(deftest test-loading-h2o-above-limit-blocked
  (let [d (rc/plan-getter-loading
           (assoc-in good-loading-request [:measured/atmosphere :h2o-ppm] 99))]
    (is (= :decision/blocked (:decision d)))
    (is (= #{:h2o} (:violated d)))))

(deftest test-loading-missing-interlocks-blocked-with-list
  (let [d (rc/plan-getter-loading
           (update good-loading-request :interlocks-present disj :class-d-extinguisher))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/missing-interlocks (:reason d)))
    (is (= [:class-d-extinguisher] (:missing-interlocks d)))))

(deftest test-loading-no-approval-waits-for-human
  (let [d (rc/plan-getter-loading (assoc good-loading-request :approvals #{}))]
    (is (= :decision/waiting-human-approval (:decision d)))
    (is (= :effect/none (:effect d)))
    (is (= #{:load-getter} (:approval-required d)))
    (is (false? (:audit/bot-commanded-equipment d)))))

(deftest test-loading-wrong-scope-approval-waits
  (let [d (rc/plan-getter-loading
           (assoc good-loading-request :approvals #{:pressurize}))]
    (is (= :decision/waiting-human-approval (:decision d)))))

;; ---------------------------------------------------------------------------
;; Activity 2 — plan hermetic closure
;; ---------------------------------------------------------------------------

(deftest test-closure-happy-path-approves-simulate-only
  (let [d (rc/plan-hermetic-closure good-closure-request)]
    (is (= :decision/approved (:decision d)))
    (is (= :effect/simulate-closure-plan (:effect d)))
    (is (= :laser-welding (get-in d [:plan :closure/joint-method])))
    (is (= :deferred-to-reactor-fab-leak-gate
           (get-in d [:plan :post-closure-leak-gate])))
    (is (false? (contains? (:plan d) :leak-verdict)))
    (is (= #{:perform-closure} (:human-approvals-used d)))
    (is (false? (:audit/bot-commanded-equipment d)))))

(deftest test-closure-bot-command-refused-even-with-approval
  (let [d (rc/plan-hermetic-closure
           (assoc good-closure-request :request/effects #{:perform-closure-live}))]
    (is (= :decision/refused (:decision d)))
    (is (= :effect/refuse (:effect d)))))

(deftest test-closure-missing-serial-refused
  (let [d (rc/plan-hermetic-closure (assoc good-closure-request :reactor/serial nil))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/missing-traceability (:reason d)))))

(deftest test-closure-unrecognized-joint-method-refused-not-normalized
  (let [d (rc/plan-hermetic-closure
           (assoc good-closure-request :closure/joint-method :glue))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/unrecognized-joint-method (:reason d)))
    (is (contains? (set (:recognized-methods d)) :vacuum-brazing))))

(deftest test-closure-unmeasured-inputs-listed-never-invented
  (let [d (rc/plan-hermetic-closure
           (assoc good-closure-request
                  :closure/wps-record-id nil
                  :design-record/closure-temp-c nil))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/unmeasured-closure-inputs (:reason d)))
    (is (= [:closure/wps-record-id :design-record/closure-temp-c]
           (:unmeasured d)))))

(deftest test-closure-missing-interlocks-blocked-with-list
  (let [d (rc/plan-hermetic-closure
           (update good-closure-request :interlocks-present disj :no-hydrogen-service-in-area))]
    (is (= :decision/blocked (:decision d)))
    (is (= [:no-hydrogen-service-in-area] (:missing-interlocks d)))))

(deftest test-closure-no-approval-waits-for-human
  (let [d (rc/plan-hermetic-closure (assoc good-closure-request :approvals #{}))]
    (is (= :decision/waiting-human-approval (:decision d)))
    (is (= #{:perform-closure} (:approval-required d)))))

;; ---------------------------------------------------------------------------
;; Decision 3 — screen equipment offer
;; ---------------------------------------------------------------------------

(deftest test-offer-referred-to-human
  (let [d (rc/screen-equipment-offer good-offer)]
    (is (= :decision/refer-to-human (:decision d)))
    (is (= :refurbished (:condition d)))
    (is (= 184000 (get-in d [:cost-inputs :price])))
    (is (true? (:audit/human-approval-required d)))
    (is (false? (:audit/bot-commanded-equipment d)))))

(deftest test-offer-missing-values-recorded-unmeasured-not-invented
  (let [d (rc/screen-equipment-offer (dissoc good-offer :price))]
    (is (= :decision/refer-to-human (:decision d)))
    (is (= :unmeasured (get-in d [:cost-inputs :price])))
    (is (= :unmeasured (get-in d [:cost-inputs :compliance])))))

(deftest test-offer-unknown-condition-accepted-verbatim
  (let [d (rc/screen-equipment-offer (assoc good-offer :condition :unknown))]
    (is (= :decision/refer-to-human (:decision d)))
    (is (= :unknown (:condition d)))))

(deftest test-offer-bad-condition-refused-not-normalized
  (let [d (rc/screen-equipment-offer (assoc good-offer :condition :like-new))]
    (is (= :decision/refused (:decision d)))
    (is (= :blocked/malformed-offer (:reason d)))))

(deftest test-offer-non-https-source-refused
  (let [d (rc/screen-equipment-offer
           (assoc good-offer :source-url "http://insecure.example/furnace"))]
    (is (= :decision/refused (:decision d)))
    (is (= :blocked/missing-first-party-source (:reason d)))))

(deftest test-offer-missing-name-refused
  (let [d (rc/screen-equipment-offer (assoc good-offer :offer/name ""))]
    (is (= :decision/refused (:decision d)))))
