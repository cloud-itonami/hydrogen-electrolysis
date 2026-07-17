(ns hydrogen-electrolysis.methods.analyze
  "hydrogen_electrolysis — analysis entry-point. 1:1 port of methods/analyze.py.

  The Python main() calls run_comparison() (kami-sim engine — omitted, same pattern as
  electrolysis.cljc), then writes three output files under methods/out/:
    - comparison.json       (the full comparison map)
    - comparison.md         (the rendered markdown report)
    - kotoba-datoms.json    (the EAVT datom rows)

  write-outputs! receives the already-computed comparison map and writes those files,
  mirroring the Python main() body exactly. run-comparison-stub replaces the omitted
  kami-sim engine call for testing."
  #?(:clj  (:require [clojure.java.io :as io]
                     [cheshire.core :as json]
                     [hydrogen-electrolysis.methods.electrolysis :as e])
     :cljs (:require [hydrogen-electrolysis.methods.electrolysis :as e])))

;; ---------------------------------------------------------------------------
;; Stub for the omitted kami-sim engine leg (mirrors Python run_comparison)
;; ---------------------------------------------------------------------------

(defn run-comparison-stub
  "STUB: in Python this calls kami_hydrogen_electrolysis_sim.simulate_default_cases /
  rank_by_electrical_energy / scene_spec from the 40-engine/kami-engine submodule.
  Returns a representative fixture for testing. The live leg is omitted."
  ([] (run-comparison-stub 10000.0))
  ([active-area-cm2]
   {"actor"                "hydrogen_electrolysis"
    "engine"               "kami-hydrogen-electrolysis-sim"
    "active_area_cm2"      active-area-cm2
    "best_low_temperature" {"name" "cfe-zero-gap-aem-high-pressure"}
    "best_electrical"      {"name" "soec-high-temperature"}
    "results"
    [{"name"                          "cfe-zero-gap-aem-high-pressure"
      "cell_voltage_v"                1.742
      "electrical_kwh_per_kg"         46.318
      "total_with_heat_kwh_per_kg"    48.901
      "hhv_electrical_efficiency_pct" 85.12
      "hhv_total_efficiency_pct"      80.63
      "h2_kg_per_hour"                0.421337
      "output_pressure_bar"           30.0}
     {"name"                          "soec-high-temperature"
      "cell_voltage_v"                1.293
      "electrical_kwh_per_kg"         37.004
      "total_with_heat_kwh_per_kg"    52.118
      "hhv_electrical_efficiency_pct" 106.55
      "hhv_total_efficiency_pct"      75.66
      "h2_kg_per_hour"                0.298122
      "output_pressure_bar"           1.0}]}))

;; ---------------------------------------------------------------------------
;; Pure file-writing entry point (mirrors Python main() body)
;; ---------------------------------------------------------------------------

#?(:clj
   (defn write-outputs!
     "Given a `comparison` map writes three output files under `out-dir`:
       comparison.json    – full comparison map as pretty-printed JSON
       comparison.md      – rendered markdown report
       kotoba-datoms.json – EAVT datom rows as pretty-printed JSON
     Returns {:files [...paths...]}."
     [comparison out-dir]
     (let [out (io/file out-dir)]
       (.mkdirs out)
       (let [f-json  (io/file out "comparison.json")
             f-md    (io/file out "comparison.md")
             f-datom (io/file out "kotoba-datoms.json")]
         (spit f-json  (str (json/generate-string comparison {:pretty true}) "\n"))
         (spit f-md    (e/render-report comparison))
         (spit f-datom (str (json/generate-string (e/kotoba-datoms comparison) {:pretty true}) "\n"))
         {:files [(str f-json) (str f-md) (str f-datom)]}))))

;; ---------------------------------------------------------------------------
;; Main (mirrors Python if __name__ == "__main__")
;; ---------------------------------------------------------------------------

#?(:clj
   (defn -main
     "Entry point: runs the comparison stub and writes output files."
     [& _args]
     (let [comparison (run-comparison-stub)
           out-dir    (str (io/file (System/getProperty "user.dir") "out"))]
       (write-outputs! comparison out-dir)
       (println (e/render-report comparison)))))
