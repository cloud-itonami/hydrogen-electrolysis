(ns hydrogen-electrolysis.methods.test-reactor-fab
  "Tests for the hydrogen reactor fabrication cell decision contract."
  (:require [clojure.test :refer [deftest is]]
            [hydrogen-electrolysis.methods.reactor-fab :as rf]))

(def good-request
  {:activity :plan-leak-and-pressure-test
   :request/effects #{:simulate-test-plan}
   :approvals #{:pressurize :leak-test-live}
   :design-pressure-bar 30.0
   :test-medium :helium
   :interlocks-present #{:h2-detector-armed :inert-purge-procedure
                         :ventilation-confirmed :ignition-sources-cleared
                         :remote-test-area :pressure-relief-fitted
                         :barricade-exclusion-zone}})

(deftest test-approved-plan-carries-audit-tail
  (let [d (rf/plan-leak-and-pressure-test good-request)]
    (is (= :decision/approved (:decision d)))
    (is (= :effect/simulate-test-plan (:effect d)))
    (is (= :hydrogen-reactor-fabrication (:audit/cell d)))
    (is (false? (:audit/bot-commanded-equipment d)))
    (is (= #{:pressurize :leak-test-live} (:human-approvals-used d)))))

(deftest test-bot-commanding-equipment-is-refused-even-with-approval
  (let [d (rf/plan-leak-and-pressure-test
           (assoc good-request :request/effects #{:command-equipment}))]
    (is (= :decision/refused (:decision d)))
    (is (= :effect/refuse (:effect d)))
    (is (= [:command-equipment] (:audit/refused-effects d)))))

(deftest test-live-h2-admit-effect-refused
  (let [d (rf/plan-leak-and-pressure-test
           (assoc good-request :request/effects #{:pressurize-live-h2}))]
    (is (= :decision/refused (:decision d)))))

(deftest test-hydrogen-as-test-medium-refused
  (let [d (rf/plan-leak-and-pressure-test (assoc good-request :test-medium :hydrogen))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/leak-screening-is-inert-gas-only (:reason d)))))

(deftest test-unmeasured-design-pressure-blocks
  (let [d (rf/plan-leak-and-pressure-test (assoc good-request :design-pressure-bar nil))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/unmeasured-design-pressure (:reason d)))
    (is (= [:design-pressure-bar] (:unmeasured d))))
  (let [d (rf/plan-leak-and-pressure-test (assoc good-request :design-pressure-bar 0))]
    (is (= :decision/blocked (:decision d)))))

(deftest test-missing-interlocks-blocked-with-explicit-list
  (let [d (rf/plan-leak-and-pressure-test
           (update good-request :interlocks-present disj :pressure-relief-fitted))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/missing-interlocks (:reason d)))
    (is (= [:pressure-relief-fitted] (:missing-interlocks d)))))

(deftest test-no-approval-waits-for-human
  (let [d (rf/plan-leak-and-pressure-test (assoc good-request :approvals #{}))]
    (is (= :decision/waiting-human-approval (:decision d)))
    (is (= :effect/none (:effect d)))
    (is (= #{:pressurize :leak-test-live} (:approval-required d)))))

(deftest test-partial-approval-still-waits
  (let [d (rf/plan-leak-and-pressure-test (assoc good-request :approvals #{:pressurize}))]
    (is (= :decision/waiting-human-approval (:decision d)))))

(deftest test-required-interlocks-is-total-source
  (is (contains? (rf/required-interlocks) :h2-detector-armed))
  (is (contains? (rf/required-interlocks) :ignition-sources-cleared)))

(deftest test-foreign-activity-refused
  (let [d (rf/plan-leak-and-pressure-test (assoc good-request :activity :something-else))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/not-this-gate (:reason d)))))

;; --- procurement gate ------------------------------------------------------

(def good-offer
  {:offer/name "Helium leak detector, sniffer"
   :source-url "https://example-manufacturer.example/leak-detector"
   :condition :new
   :price nil :currency nil :lead-time nil
   :utility nil :safety nil :compliance nil})

(deftest test-offer-defers-to-human-with-unmeasured-cost-inputs
  (let [d (rf/screen-equipment-offer good-offer)]
    (is (= :decision/refer-to-human (:decision d)))
    (is (true? (:audit/human-approval-required d)))
    (is (false? (:audit/bot-commanded-equipment d)))
    (is (every? #{:unmeasured} (vals (:cost-inputs d))))
    (is (= :new (:condition d)))))

(deftest test-used-condition-is-recorded-not-normalized
  (let [d (rf/screen-equipment-offer (assoc good-offer :condition :used))]
    (is (= :used (:condition d))))
  (let [d (rf/screen-equipment-offer (assoc good-offer :condition :refurbished))]
    (is (= :refurbished (:condition d)))))

(deftest test-unknown-condition-preserved
  (let [d (rf/screen-equipment-offer (assoc good-offer :condition :unknown))]
    (is (= :decision/refer-to-human (:decision d)))
    (is (= :unknown (:condition d)))))

(deftest test-malformed-condition-rejected
  (let [d (rf/screen-equipment-offer (assoc good-offer :condition "brand-new"))]
    (is (= :decision/refused (:decision d)))
    (is (= :blocked/malformed-offer (:reason d)))))

(deftest test-offer-without-first-party-source-refused
  (let [d (rf/screen-equipment-offer (assoc good-offer :source-url "http://x.example"))]
    (is (= :decision/refused (:decision d)))
    (is (= :blocked/missing-first-party-source (:reason d)))))

(deftest test-offer-without-name-refused
  (let [d (rf/screen-equipment-offer (dissoc good-offer :offer/name))]
    (is (= :decision/refused (:decision d)))))

(deftest test-observed-values-carried-not-replaced
  (let [d (rf/screen-equipment-offer
           (assoc good-offer :price 12000 :currency "USD" :lead-time "8-12 wks"))]
    (is (= 12000 (get-in d [:cost-inputs :price])))
    (is (= "USD" (get-in d [:cost-inputs :currency])))
    (is (= :unmeasured (get-in d [:cost-inputs :safety])))))
