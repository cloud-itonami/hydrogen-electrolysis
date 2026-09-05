#!/usr/bin/env bb
(require '[clojure.test :as test])

(def suites
  '[hydrogen-electrolysis.methods.test-electrolysis
    hydrogen-electrolysis.methods.test-analyze
    hydrogen-electrolysis.methods.test-reactor-fab
    hydrogen-electrolysis.methods.test-reactor-closure
    hydrogen-electrolysis.methods.test-reactor-weld
    hydrogen-electrolysis.kotoba.test-ingest-efficiency
    hydrogen-electrolysis.kotoba.deploy-test])

(apply require suites)
(let [{:keys [fail error]} (apply test/run-tests suites)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
    hydrogen-electrolysis.methods.test-reactor-servicing
    hydrogen-electrolysis.methods.test-cartridge-fill
