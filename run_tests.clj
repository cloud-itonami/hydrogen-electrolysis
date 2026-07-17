#!/usr/bin/env bb
(require '[clojure.test :as test])

(def suites
  '[hydrogen-electrolysis.methods.test-electrolysis
    hydrogen-electrolysis.methods.test-analyze
    hydrogen-electrolysis.kotoba.test-ingest-efficiency
    hydrogen-electrolysis.kotoba.deploy-test])

(apply require suites)
(let [{:keys [fail error]} (apply test/run-tests suites)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
