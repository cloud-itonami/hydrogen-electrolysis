(ns hydrogen-electrolysis.methods.analyze
  "hydrogen_electrolysis — analysis entry-point. 1:1 port of methods/analyze.py.

  The live kami-sim engine leg is omitted. The standalone actor writes canonical
  EDN comparison data and EAVT datoms plus a derived Markdown report.
  run-comparison-stub supplies the deterministic offline fixture."
  #?(:clj  (:require [clojure.java.io :as io]
                     [clojure.pprint :as pprint]
                     [hydrogen-electrolysis.methods.electrolysis :as e])
     :cljs (:require [hydrogen-electrolysis.methods.electrolysis :as e])))

;; ---------------------------------------------------------------------------
;; Stub for the omitted kami-sim engine leg (mirrors Python run_comparison)
;; ---------------------------------------------------------------------------

(defn run-comparison-stub
  "STUB: in Python this calls kami_hydrogen_electrolysis_sim.simulate_default_cases /
  rank_by_electrical_energy / scene_spec from the pinned Kami Engine checkout.
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
     "Given a `comparison` map writes canonical comparison.edn and
     kotoba-datoms.edn plus the derived comparison-report.md.
     Returns {:files [...paths...]}."
     [comparison out-dir]
     (let [out (io/file out-dir)]
       (.mkdirs out)
       (let [f-edn   (io/file out "comparison.edn")
             f-md    (io/file out "comparison-report.md")
             f-datom (io/file out "kotoba-datoms.edn")]
         (with-open [writer (io/writer f-edn)]
           (binding [*out* writer] (pprint/pprint comparison)))
         (spit f-md (e/render-report comparison))
         (with-open [writer (io/writer f-datom)]
           (binding [*out* writer] (pprint/pprint (e/kotoba-datoms comparison))))
         {:files [(str f-edn) (str f-md) (str f-datom)]}))))

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
