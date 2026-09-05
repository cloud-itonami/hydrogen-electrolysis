(ns hydrogen-electrolysis.methods.test-reactor-weld
  "Tests for the reactor joint-fabrication decision contract."
  (:require [clojure.test :refer [deftest is]]
            [hydrogen-electrolysis.methods.reactor-weld :as rw]))

(def good-request
  {:activity :plan-joint-fabrication
   :request/effects #{:simulate-joint-plan}
   :approvals #{:perform-weld}
   :joint/id "J-001"
   :wps-id "WPS-H2-001"
   :base-material "SS316L"
   :filler "ER316L"
   :service-pressure-bar 30.0
   :inspection-set #{:visual :dye-penetrant :radiography}
   :operator-qualified true})

(deftest test-approved-plan-carries-audit-tail
  (let [d (rw/plan-joint-fabrication good-request)]
    (is (= :decision/approved (:decision d)))
    (is (= :effect/simulate-joint-plan (:effect d)))
    (is (= :hydrogen-reactor-fabrication (:audit/cell d)))
    (is (false? (:audit/bot-commanded-equipment d)))
    (is (= #{:perform-weld} (:human-approvals-used d)))
    (is (= "WPS-H2-001" (:wps-id d)))
    (is (= "ER316L" (:filler d)))))

(deftest test-bot-commanding-weld-refused-even-with-approval
  (doseq [fx #{:perform-weld-live :weld-live :command-equipment}]
    (let [d (rw/plan-joint-fabrication
             (assoc good-request :request/effects #{fx}))]
      (is (= :decision/refused (:decision d)))
      (is (= :effect/refuse (:effect d)))
      (is (= [fx] (:audit/refused-effects d))))))

(deftest test-no-procedure-named-is-out-of-scope-not-filled-in
  (let [d (rw/plan-joint-fabrication (assoc good-request :wps-id nil))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/no-procedure-specified (:reason d))))
  (let [d (rw/plan-joint-fabrication (assoc good-request :wps-id ""))]
    (is (= :decision/refused (:decision d)))))

(deftest test-unmeasured-base-material-blocks
  (let [d (rw/plan-joint-fabrication (assoc good-request :base-material nil))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/unmeasured-base-material (:reason d)))
    (is (= [:base-material] (:unmeasured d)))))

(deftest test-unmeasured-service-pressure-blocks
  (let [d (rw/plan-joint-fabrication (assoc good-request :service-pressure-bar nil))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/unmeasured-service-pressure (:reason d)))))

(deftest test-pressure-service-without-inspection-set-blocked
  (let [d (rw/plan-joint-fabrication (assoc good-request :inspection-set #{}))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/inspection-set-required (:reason d))))
  ;; zero-pressure (non-pressurized) joint may pass without inspection set
  (let [d (rw/plan-joint-fabrication
           (assoc good-request :service-pressure-bar 0 :inspection-set #{}))]
    (is (= :decision/approved (:decision d)))))

(deftest test-unknown-inspection-method-blocked
  (let [d (rw/plan-joint-fabrication
           (assoc good-request :inspection-set #{:visual :magic-scan}))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/unknown-inspection-method (:reason d)))
    (is (= [:magic-scan] (:unknown-inspection d)))))

(deftest test-nil-filler-recorded-unmeasured-not-defaulted
  (let [d (rw/plan-joint-fabrication (assoc good-request :filler nil))]
    (is (= :decision/approved (:decision d)))
    (is (= :unmeasured (:filler d)))))

(deftest test-unqualified-operator-blocks-even-with-approval
  (let [d (rw/plan-joint-fabrication (assoc good-request :operator-qualified false))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/operator-qualification-unverified (:reason d))))
  (let [d (rw/plan-joint-fabrication (assoc good-request :operator-qualified nil))]
    (is (= :decision/blocked (:decision d)))
    (is (= :blocked/operator-qualification-unverified (:reason d)))))

(deftest test-no-approval-waits-for-human
  (let [d (rw/plan-joint-fabrication (assoc good-request :approvals #{}))]
    (is (= :decision/waiting-human-approval (:decision d)))
    (is (= :effect/none (:effect d)))
    (is (= #{:perform-weld} (:approval-required d)))))

(deftest test-foreign-activity-refused
  (let [d (rw/plan-joint-fabrication (assoc good-request :activity :weld-it))]
    (is (= :decision/refused (:decision d)))
    (is (= :reason/not-this-gate (:reason d)))))

(deftest test-hazardous-activity-set-exposed
  (is (= #{:perform-weld} (rw/joint-requires-approval))))
