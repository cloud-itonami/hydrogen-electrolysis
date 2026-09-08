(ns hydrogen-electrolysis.methods.test-reactor-control
  "Tests for the hydrogen reactor operational-control cell decision contract
  (activity → decision → effect → audit). Pure; deterministic; stdlib only."
  (:require [clojure.test :refer [deftest is]]
            [hydrogen-electrolysis.methods.reactor-control :as rc]))

(def good-operation
  {:reactor/serial "RX-2026-0001"
   :cartridge/serial "CARTRIDGE-2026-0007"
   :cartridge/condition :new
   :request/effects #{:simulate-operation-plan}
   :approvals #{:load-cartridge :pressurize}
   :design-record/max-operating-pressure-bar 12.0
   :design-record/operating-pressure-band {:min-bar 2.0 :max-bar 8.0}
   :measured/flow-demand-lmin 5.0
   :interlocks-present #{:h2-detector-calibrated
                         :overpressure-relief-verified
                         :flame-arrestor-fitted
                         :grounding-bonding-verified
                         :vent-line-cleared
                         :dry-inert-cartridge-handling}})

(def good-leak
  {:reactor/serial "RX-2026-0001"
   :design-record/max-leak-rate-unit 5.0
   :measured/leak-rate {:value 0.8 :unit "sccm"}
   :measured/ambient-h2-ppm 12
   :request/effects #{:evaluate-leak-interlock}})

(def good-termination
  {:reactor/serial "RX-2026-0001"
   :request/effects #{:simulate-safe-state-termination}
   :approvals #{:vent :purge}
   :interlocks-present #{:vent-line-cleared :flame-arrestor-fitted
                         :grounding-bonding-verified}})

;; ── plan-operation ─────────────────────────────────────────────────────────

(deftest test-operation-approved-with-audit-tail
  (let [d (rc/plan-operation good-operation)]
    (is (= :decision/approved (:decision d)))
    (is (= :effect/simulate-operation-plan (:effect d)))
    (is (= :hydrogen-reactor-control (:audit/cell d)))
    (is (false? (:audit/bot-commanded-equipment d)))
    (is (= #{:load-cartridge :pressurize} (:human-approvals-used d)))))

(deftest test-operation-bot-command-refused-even-with-approval
  (let [d (rc/plan-operation
           (assoc good-operation :request/effects #{:command-equipment}))]
    (is (= :decision/refused (:decision d)))
    (is (= :effect/refuse (:effect d)))
    (is (= [:command-equipment] (:audit/refused-effects d)))))

(deftest test-operation-live-h2-vent-activate-refused
  (doseq [fx [#{:pressurize-live-h2} #{:vent-live-h2} #{:activate-valve-live}]]
    (is (= :decision/refused
           (:decision (rc/plan-operation (assoc good-operation :request/effects fx)))))))

(deftest test-operation-missing-traceability-refused
  (let [d (rc/plan-operation (dissoc good-operation :reactor/serial))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/missing-traceability (:reason d)))))

(deftest test-operation-cartridge-condition-must-be-recognized
  (let [d (rc/plan-operation (assoc good-operation :cartridge/condition :recycled))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/unrecognized-cartridge-condition (:reason d)))
    (is (some #{:new :used :refurbished :unknown}
              (:recognized-conditions d)))))

(deftest test-operation-unknown-condition-is-legal-recorded-value
  (let [d (rc/plan-operation (assoc good-operation :cartridge/condition :unknown))]
    (is (= :decision/approved (:decision d)))
    (is (= :unknown (get-in d [:plan :cartridge/condition])))))

(deftest test-operation-missing-measured-inputs-blocked-not-invented
  (let [d (rc/plan-operation (dissoc good-operation :design-record/max-operating-pressure-bar))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/unmeasured-operation-inputs (:reason d)))
    (is (some #{:design-record/max-operating-pressure-bar} (:unmeasured d))))
  (let [d (rc/plan-operation (dissoc good-operation :measured/flow-demand-lmin))]
    (is (= :decision/blocked (:decision d)))
    (is (some #{:measured/flow-demand-lmin} (:unmeasured d)))))

(deftest test-operation-demand-above-licensed-band-blocks
  (let [d (rc/plan-operation (assoc good-operation :measured/flow-demand-lmin 55.0))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/demand-exceeds-licensed-band (:reason d)))))

(deftest test-operation-missing-interlocks-blocks
  (let [d (rc/plan-operation
           (assoc good-operation :interlocks-present
                  #{:h2-detector-calibrated :overpressure-relief-verified}))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/missing-interlocks (:reason d)))
    (is (some #{:dry-inert-cartridge-handling} (:missing-interlocks d)))))

(deftest test-operation-without-human-approval-defers-never-approves
  (let [d (rc/plan-operation (assoc good-operation :approvals #{}))]
    (is (= :decision/waiting-human-approval (:decision d)))
    (is (= #{:load-cartridge :pressurize} (:approval-required d))))
  (let [d (rc/plan-operation (assoc good-operation :approvals #{:load-cartridge}))]
    (is (= :decision/waiting-human-approval (:decision d)))))

;; ── evaluate-leak-interlock ────────────────────────────────────────────────

(deftest test-leak-clear-to-operate-simulated
  (let [d (rc/evaluate-leak-interlock good-leak)]
    (is (= :decision/clear-to-operate (:decision d)))
    (is (= :effect/simulate-continue-operation (:effect d)))
    (is (false? (:audit/bot-commanded-equipment d)))))

(deftest test-leak-detector-offline-safe-state-vent-simulated
  (let [d (rc/evaluate-leak-interlock (assoc good-leak :measured/ambient-h2-ppm nil))]
    (is (= :decision/safe-state-and-vent (:decision d)))
    (is (= :reason/h2-detector-offline (:reason d)))
    (is (= :effect/simulate-safe-state-vent (:effect d)))
    (is (= :de-pressurized-and-vented (:target-safe-state d)))))

(deftest test-leak-above-design-limit-safe-state-vent
  (let [d (rc/evaluate-leak-interlock
           (assoc-in good-leak [:measured/leak-rate :value] 9.0))]
    (is (= :decision/safe-state-and-vent (:decision d)))
    (is (= :reason/leak-exceeds-design-limit (:reason d)))
    (is (= 0.8 (:value (get-in good-leak [:measured/leak-rate]))))))

(deftest test-leak-missing-measured-inputs-blocked
  (let [d (rc/evaluate-leak-interlock (dissoc good-leak :design-record/max-leak-rate-unit))]
    (is (= :decision/blocked (:decision d)))
    (is (some #{:design-record/max-leak-rate-unit} (:unmeasured d))))
  (let [d (rc/evaluate-leak-interlock (dissoc good-leak :measured/leak-rate))]
    (is (= :decision/blocked (:decision d)))
    (is (some #{:measured/leak-rate} (:unmeasured d)))))

(deftest test-leak-command-refused
  (let [d (rc/evaluate-leak-interlock
           (assoc good-leak :request/effects #{:vent-live-h2}))]
    (is (= :decision/refused (:decision d)))
    (is (= [:vent-live-h2] (:audit/refused-effects d)))))

;; ── plan-safe-state-termination ────────────────────────────────────────────

(deftest test-safe-state-termination-approved-simulated
  (let [d (rc/plan-safe-state-termination good-termination)]
    (is (= :decision/approved (:decision d)))
    (is (= :effect/simulate-safe-state-termination (:effect d)))
    (is (= [:inert-purge :vent-to-atmosphere :shutdown] (:sequence (:plan d))))))

(deftest test-termination-command-refused
  (let [d (rc/plan-safe-state-termination
           (assoc good-termination :request/effects #{:vent-live-h2}))]
    (is (= :decision/refused (:decision d)))))

(deftest test-termination-missing-interlocks-blocks
  (let [d (rc/plan-safe-state-termination
           (assoc good-termination :interlocks-present #{:vent-line-cleared}))]
    (is (= :decision/blocked (:decision d)))
    (is (some #{:flame-arrestor-fitted} (:missing-interlocks d)))))

(deftest test-termination-without-approval-defers
  (let [d (rc/plan-safe-state-termination
           (assoc good-termination :approvals #{}))]
    (is (= :decision/waiting-human-approval (:decision d)))
    (is (= #{:vent :purge} (:approval-required d)))))

;; ── screen-equipment-offer ─────────────────────────────────────────────────

(deftest test-offer-always-referred-to-human
  (let [d (rc/screen-equipment-offer
           {:offer/name "H2 mass-flow controller"
            :source-url "https://example.instrument.example/mfc-200"
            :condition :used})]
    (is (= :decision/refer-to-human (:decision d)))
    (is (false? (:audit/bot-commanded-equipment d)))
    (is (true? (:audit/human-approval-required d)))
    (is (= :unmeasured (get-in d [:cost-inputs :price])))
    (is (= :unmeasured (get-in d [:cost-inputs :lead-time])))))

(deftest test-offer-condition-must-be-distinguished
  (let [d (rc/screen-equipment-offer
           {:offer/name "H2 detector" :source-url "https://x.example/det"
            :condition :new-in-box})]
    (is (= :decision/refused (:decision d)))
    (is (= :blocked/malformed-offer (:reason d)))))

(deftest test-offer-requires-first-party-source
  (let [d (rc/screen-equipment-offer
           {:offer/name "regulator" :condition :used})]
    (is (= :decision/refused (:decision d)))
    (is (= :blocked/missing-first-party-source (:reason d)))))

(deftest test-offer-keeps-observed-price-recorded
  (let [d (rc/screen-equipment-offer
           {:offer/name "pressure regulator"
            :source-url "https://dealer.example/reg-300"
            :condition :refurbished :price 45000 :currency "JPY"})]
    (is (= :decision/refer-to-human (:decision d)))
    (is (= 45000 (get-in d [:cost-inputs :price])))
    (is (= "JPY" (get-in d [:cost-inputs :currency])))))