#!/usr/bin/env bash
# hydrogen_electrolysis — clj/bb test suite (ADR-2606160842 py->clj port wave); wired into the fleet green-check.
set -euo pipefail
cd "$(dirname "$0")/../.."
exec bb -e '
(require
  (quote clojure.test)
  (quote hydrogen-electrolysis.methods.test-electrolysis)
  (quote hydrogen-electrolysis.methods.test-analyze)
  (quote hydrogen-electrolysis.kotoba.test-ingest-efficiency))
(let [r (clojure.test/run-tests
          (quote hydrogen-electrolysis.methods.test-electrolysis)
          (quote hydrogen-electrolysis.methods.test-analyze)
          (quote hydrogen-electrolysis.kotoba.test-ingest-efficiency))]
  (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))'
