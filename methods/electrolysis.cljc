(ns hydrogen-electrolysis.methods.electrolysis
  "hydrogen_electrolysis — water-electrolysis efficiency comparison readout. 1:1 port of the PURE
  leaf functions of methods/electrolysis.py: kotoba-datoms (comparison → EAVT datoms) and
  render-report (comparison → markdown report). Both operate on a plain `comparison` map (string
  keys mirroring the Python dict), so they are pure-stdlib and need no engine.

  The producer `run_comparison` is the OMITTED leg: it imports the kami-hydrogen-electrolysis-sim
  engine from the 40-engine/kami-engine submodule (simulate_default_cases / rank_by_electrical_energy
  / scene_spec) — a cross-engine kami-sim closure, the analog of an omitted network/live leg. The
  comparison map it returns is supplied here as test fixtures."
  (:require [clojure.string :as str]))

(defn- roundn [x n]
  (let [f (Math/pow 10.0 n)]
    (/ (Math/round (* (double x) f)) f)))

(defn kotoba-datoms
  "comparison map → vector of EAVT datom maps (string keys mirror the Python dict)."
  [comparison]
  (let [rows (mapv (fn [result]
                     {":db/id" (str "hydrogen-electrolysis/" (get result "name"))
                      ":hydrogen.electrolysis/name" (get result "name")
                      ":hydrogen.electrolysis/actor" (get comparison "actor")
                      ":hydrogen.electrolysis/engine" (get comparison "engine")
                      ":hydrogen.electrolysis/electrical-kwh-per-kg-h2" (roundn (get result "electrical_kwh_per_kg") 4)
                      ":hydrogen.electrolysis/total-with-heat-kwh-per-kg-h2" (roundn (get result "total_with_heat_kwh_per_kg") 4)
                      ":hydrogen.electrolysis/hhv-electrical-efficiency-pct" (roundn (get result "hhv_electrical_efficiency_pct") 3)
                      ":hydrogen.electrolysis/hhv-total-efficiency-pct" (roundn (get result "hhv_total_efficiency_pct") 3)
                      ":hydrogen.electrolysis/h2-kg-per-hour" (roundn (get result "h2_kg_per_hour") 6)
                      ":hydrogen.electrolysis/output-pressure-bar" (get result "output_pressure_bar")})
                   (get comparison "results"))]
    (conj rows
          {":db/id" "hydrogen-electrolysis/recommendation/low-temperature"
           ":hydrogen.electrolysis/recommended-case" (get-in comparison ["best_low_temperature" "name"])
           ":hydrogen.electrolysis/rationale" "capillary-feed + zero-gap AEM + high-pressure minimizes bubble, ohmic, and compression losses"})))

(defn render-report
  "comparison map → markdown efficiency-comparison report."
  [comparison]
  (let [head [(str "# hydrogen_electrolysis — efficiency comparison")
              ""
              (str "- actor: `" (get comparison "actor") "`")
              (str "- simulation engine: `" (get comparison "engine") "`")
              (str "- active area: `" (format "%.0f" (double (get comparison "active_area_cm2"))) " cm^2`")
              (str "- best low-temperature candidate: `" (get-in comparison ["best_low_temperature" "name"]) "`")
              (str "- lowest electrical energy candidate: `" (get-in comparison ["best_electrical" "name"]) "`")
              ""
              "| case | cell V | electrical kWh/kg-H2 | heat-inclusive kWh/kg-H2 | HHV electrical % | H2 kg/h | pressure bar |"
              "|---|---:|---:|---:|---:|---:|---:|"]
        rows (mapv (fn [r]
                     (format "| %s | %.3f | %.2f | %.2f | %.1f | %.3f | %.0f |"
                             (get r "name")
                             (double (get r "cell_voltage_v"))
                             (double (get r "electrical_kwh_per_kg"))
                             (double (get r "total_with_heat_kwh_per_kg"))
                             (double (get r "hhv_electrical_efficiency_pct"))
                             (double (get r "h2_kg_per_hour"))
                             (double (get r "output_pressure_bar"))))
                   (get comparison "results"))
        tail ["" (str "Interpretation: SOEC can minimize electrical input when useful heat is available. "
                      "For low-temperature water electrolysis, the strongest candidate is "
                      "`cfe-zero-gap-aem-high-pressure` because it combines capillary bubble suppression, "
                      "short ion path, AEM stack economics, and reduced downstream compression.")]]
    (str (str/join "\n" (concat head rows tail)) "\n")))
