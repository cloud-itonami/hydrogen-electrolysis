(ns hydrogen-electrolysis.kotoba.test-ingest-efficiency
  "Tests for ingest_efficiency.cljc — the pure-logic port of kotoba/ingest_efficiency.py.
  Covers: claim helper, entities shape for both Case and Recommendation rows.
  IO legs (HTTP, subprocess) are omitted in the cljc port and not tested here."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
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

(deftest test-entities-edn-roundtrip
  (let [entities (ie/entities (e/kotoba-datoms comparison))]
    (is (= entities (edn/read-string (pr-str entities))))))
