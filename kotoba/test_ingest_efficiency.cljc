(ns hydrogen-electrolysis.kotoba.test-ingest-efficiency
  "Tests for ingest_efficiency.cljc — the pure-logic port of kotoba/ingest_efficiency.py.
  Covers: claim helper, entities shape for both Case and Recommendation rows.
  IO legs (HTTP, subprocess) are omitted in the cljc port and not tested here."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            #?(:clj [cheshire.core :as json])
            [hydrogen-electrolysis.kotoba.ingest-efficiency :as ie]
            [hydrogen-electrolysis.methods.electrolysis :as e]))

(def ^:private comparison
  {"actor"                "hydrogen_electrolysis"
   "engine"               "kami-hydrogen-electrolysis-sim"
   "active_area_cm2"      10000.0
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
     "output_pressure_bar"           1.0}]})

;; ---------------------------------------------------------------------------
;; claim helper
;; ---------------------------------------------------------------------------

(deftest test-claim-shape
  (let [c (ie/claim "case-name" "my-case")]
    (is (= "case-name" (get c "pred")))
    (is (= "my-case"   (get c "value")))))

(deftest test-claim-coerces-to-string
  (let [c (ie/claim "output-pressure-bar" 30.0)]
    (is (= "30.0" (get c "value")))))

;; ---------------------------------------------------------------------------
;; entities — HydrogenElectrolysisCase rows
;; ---------------------------------------------------------------------------

(deftest test-entities-count
  ;; 2 case rows + 1 recommendation row → 3 entities
  (let [datoms   (e/kotoba-datoms comparison)
        entities (ie/entities datoms)]
    (is (= 3 (count entities)))))

(deftest test-case-entity-shape
  (let [datoms   (e/kotoba-datoms comparison)
        entities (ie/entities datoms)
        case-ent (first (filter #(= "HydrogenElectrolysisCase" (get % "type")) entities))]
    (is (some? case-ent))
    (is (= "hydrogen-electrolysis/cfe-zero-gap-aem-high-pressure" (get case-ent "id")))
    (is (= "0.95"   (get case-ent "confidence")))
    (is (= "CC0-1.0" (get case-ent "license")))
    (is (= "hydrogen_electrolysis actor" (get case-ent "sourceId")))
    (is (vector? (get case-ent "claims")))
    (is (= [] (get case-ent "relations")))))

(deftest test-case-entity-claims
  (let [datoms   (e/kotoba-datoms comparison)
        entities (ie/entities datoms)
        case-ent (first (filter #(= "HydrogenElectrolysisCase" (get % "type")) entities))
        claims   (get case-ent "claims")
        pred-set (set (map #(get % "pred") claims))]
    (is (pred-set "case-name"))
    (is (pred-set "actor"))
    (is (pred-set "engine"))
    (is (pred-set "electrical-kwh-per-kg-h2"))
    (is (pred-set "output-pressure-bar"))))

;; ---------------------------------------------------------------------------
;; entities — HydrogenElectrolysisRecommendation row
;; ---------------------------------------------------------------------------

(deftest test-recommendation-entity-shape
  (let [datoms   (e/kotoba-datoms comparison)
        entities (ie/entities datoms)
        rec-ent  (first (filter #(= "HydrogenElectrolysisRecommendation" (get % "type")) entities))]
    (is (some? rec-ent))
    (is (= "hydrogen-electrolysis/recommendation/low-temperature" (get rec-ent "id")))
    (is (= "Hydrogen electrolysis low-temperature recommendation" (get rec-ent "labelEn")))
    (let [claims   (get rec-ent "claims")
          pred-set (set (map #(get % "pred") claims))]
      (is (pred-set "recommended-case"))
      (is (pred-set "rationale")))))

(deftest test-recommendation-recommended-case-value
  (let [datoms   (e/kotoba-datoms comparison)
        entities (ie/entities datoms)
        rec-ent  (first (filter #(= "HydrogenElectrolysisRecommendation" (get % "type")) entities))
        claims   (get rec-ent "claims")
        rec-case (first (filter #(= "recommended-case" (get % "pred")) claims))]
    (is (= "cfe-zero-gap-aem-high-pressure" (get rec-case "value")))))

;; ---------------------------------------------------------------------------
;; entities — skip rows without :db/id
;; ---------------------------------------------------------------------------

(deftest test-entities-skips-rows-without-id
  (let [datoms   [{":db/id" ""
                   ":hydrogen.electrolysis/name" "ghost"}
                  {":db/id" "hydrogen-electrolysis/real"
                   ":hydrogen.electrolysis/name" "real"}]
        entities (ie/entities datoms)]
    (is (= 1 (count entities)))
    (is (= "hydrogen-electrolysis/real" (get (first entities) "id")))))

;; ---------------------------------------------------------------------------
;; Parity smoke: Python vs cljc (graceful skip if Python deps unavailable)
;; ---------------------------------------------------------------------------

#?(:clj
   (deftest test-parity-smoke-py-vs-clj
     ;; Runs the Python _entities() and compares entity-id set with the cljc output.
     ;; Skipped gracefully if Python deps are unavailable.
     (let [py-result
           (try
             (let [proc (-> (ProcessBuilder.
                             ["python3" "-c"
                              (str "import sys, json;"
                                   "sys.path.insert(0,'20-actors/hydrogen_electrolysis/methods');"
                                   "sys.path.insert(0,'20-actors/hydrogen_electrolysis/kotoba');"
                                   "from ingest_efficiency import _entities;"
                                   "from electrolysis import kotoba_datoms, run_comparison;"
                                   "print(json.dumps(_entities(kotoba_datoms(run_comparison()))));")])
                            (.start))
                   stdout (slurp (.getInputStream proc))
                   exit   (.waitFor proc)]
               (if (zero? exit)
                 {:ok true :data (json/parse-string stdout)}
                 {:ok false :reason (str "python3 exit " exit)}))
             (catch Exception ex
               {:ok false :reason (str "python3 unavailable: " (.getMessage ex))}))
           datoms   (e/kotoba-datoms
                     {"actor"                "hydrogen_electrolysis"
                      "engine"               "kami-hydrogen-electrolysis-sim"
                      "active_area_cm2"      10000.0
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
                        "output_pressure_bar"           1.0}]})
           clj-entities (ie/entities datoms)
           clj-id-set   (set (map #(get % "id") clj-entities))]
       (if (:ok py-result)
         (let [py-entities (:data py-result)
               py-id-set   (set (map #(get % "id") py-entities))]
           (println "PARITY check: py-ids=" py-id-set " clj-ids=" clj-id-set)
           (is (= py-id-set clj-id-set) "Python and cljc entity id sets must match"))
         (println "PARITY SKIP:" (:reason py-result))))))
