(ns hydrogen-electrolysis.methods.test-electrolysis
  "Tests for the PURE leaf functions of hydrogen_electrolysis electrolysis.cljc. 1:1 port of the
  kotoba_datoms + render_report assertions of methods/test_electrolysis.py. The Python tests drive
  these via run_comparison() (the omitted kami-sim engine leg); here a fixture `comparison` map of
  the same shape (the engine's documented output) feeds the pure functions directly."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [hydrogen-electrolysis.methods.electrolysis :as e]))

(def comparison
  {"actor" "hydrogen_electrolysis"
   "engine" "kami-hydrogen-electrolysis-sim"
   "active_area_cm2" 10000.0
   "best_low_temperature" {"name" "cfe-zero-gap-aem-high-pressure"}
   "best_electrical" {"name" "soec-high-temperature"}
   "results" [{"name" "cfe-zero-gap-aem-high-pressure" "cell_voltage_v" 1.742
               "electrical_kwh_per_kg" 46.318 "total_with_heat_kwh_per_kg" 48.901
               "hhv_electrical_efficiency_pct" 85.12 "hhv_total_efficiency_pct" 80.63
               "h2_kg_per_hour" 0.421337 "output_pressure_bar" 30.0}
              {"name" "soec-high-temperature" "cell_voltage_v" 1.293
               "electrical_kwh_per_kg" 37.004 "total_with_heat_kwh_per_kg" 52.118
               "hhv_electrical_efficiency_pct" 106.55 "hhv_total_efficiency_pct" 75.66
               "h2_kg_per_hour" 0.298122 "output_pressure_bar" 1.0}]})

(deftest test-kotoba-datoms-include-recommendation
  (let [datoms (e/kotoba-datoms comparison)]
    (is (some #(= "cfe-zero-gap-aem-high-pressure"
                  (get % ":hydrogen.electrolysis/recommended-case")) datoms))))

(deftest test-kotoba-datoms-round-and-carry-fields
  (let [row (first (e/kotoba-datoms comparison))]
    (is (= "hydrogen-electrolysis/cfe-zero-gap-aem-high-pressure" (get row ":db/id")))
    (is (= "kami-hydrogen-electrolysis-sim" (get row ":hydrogen.electrolysis/engine")))
    (is (= 46.318 (get row ":hydrogen.electrolysis/electrical-kwh-per-kg-h2")))
    (is (= 0.421337 (get row ":hydrogen.electrolysis/h2-kg-per-hour")))
    (is (= 30.0 (get row ":hydrogen.electrolysis/output-pressure-bar")))))

(deftest test-report-renders-table
  (let [report (e/render-report comparison)]
    (is (str/includes? report "efficiency comparison"))
    (is (str/includes? report "electrical kWh/kg-H2"))
    (is (str/includes? report "| cfe-zero-gap-aem-high-pressure | 1.742 | 46.32 |"))))

(deftest test-report-carries-recommendation-and-area
  (let [report (e/render-report comparison)]
    (is (str/includes? report "best low-temperature candidate: `cfe-zero-gap-aem-high-pressure`"))
    (is (str/includes? report "active area: `10000 cm^2`"))
    (is (str/includes? report "cfe-zero-gap-aem-high-pressure` because it combines"))))
