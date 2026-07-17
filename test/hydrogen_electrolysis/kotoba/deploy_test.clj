(ns hydrogen-electrolysis.kotoba.deploy-test
  (:require [clojure.test :refer [deftest is]]
            [hydrogen-electrolysis.kotoba.deploy :as deploy]))

(deftest deterministic-dry-run-plan
  (let [a (deploy/plan)
        b (deploy/plan)]
    (is (= a b))
    (is (= :dry-run (:mode a)))
    (is (true? (:operator-required a)))
    (is (= 3 (:datom-count a)))
    (is (= 3 (:entity-count a)))))
