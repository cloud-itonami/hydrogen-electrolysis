(ns hydrogen-electrolysis.kotoba.ingest-efficiency
  "hydrogen_electrolysis — kotoba KG entity shape builder. Partial port of
  kotoba/ingest_efficiency.py.

  PORTED (pure logic):
    `entities`  — datom rows → KG entity maps (the `_entities` function)
    `claim`     — (pred value) → claim map (the `_claim` helper)

  OMITTED (IO / network / subprocess — not porteable as pure-logic cljc):
    `_operator_did`       — calls `kotoba whoami` subprocess
    `_jwt_for_sub`        — constructs a JWT; pure but auth-infrastructure, not domain logic
    `_token`              — reads KOTOBA_TOKEN env + `_jwt_for_sub`; IO
    `_post_json`          — urllib.request HTTP POST; network IO
    `_post_ingest_batch`  — calls `_post_json`; network IO
    `_delete_entities`    — calls `_post_json`; network IO
    `main`                — argparse CLI + all the above IO legs; operator-gated (no-server-key)

  The pure-logic `entities` function is the only computable part testable without the live
  kotoba engine. The omitted legs are network/subprocess IO and are correctly left in Python
  as the operator tooling layer (no-server-key invariant: the operator drives submission)."
  (:require [clojure.string :as str]))

(defn claim
  "Helper: (pred value) → {:pred pred :value (str value)}.
  1:1 port of Python _claim."
  [pred value]
  {"pred"  pred
   "value" (str value)})

(defn entities
  "datom rows → vector of KG entity maps ready for the kotobase.kg.ingest_batch API.
  1:1 port of Python _entities.

  Each datom row is a string-keyed map as produced by
  `hydrogen-electrolysis.methods.electrolysis/kotoba-datoms`.
  Rows without a `:db/id` key are skipped (mirrors Python behaviour).
  Recommendation rows (containing `:hydrogen.electrolysis/recommended-case`)
  are emitted as HydrogenElectrolysisRecommendation entities;
  all other rows are emitted as HydrogenElectrolysisCase entities."
  [datoms]
  (reduce
   (fn [acc row]
     (let [entity-id (str (get row ":db/id" ""))]
       (if (str/blank? entity-id)
         acc
         (if (contains? row ":hydrogen.electrolysis/recommended-case")
           (conj acc
                 {"id"         entity-id
                  "type"       "HydrogenElectrolysisRecommendation"
                  "labelEn"    "Hydrogen electrolysis low-temperature recommendation"
                  "confidence" "0.95"
                  "license"    "CC0-1.0"
                  "sourceId"   "hydrogen_electrolysis actor"
                  "claims"     [(claim "recommended-case"
                                       (get row ":hydrogen.electrolysis/recommended-case"))
                                (claim "rationale"
                                       (get row ":hydrogen.electrolysis/rationale"))]
                  "relations"  []})
           (let [name-val (str (get row ":hydrogen.electrolysis/name" entity-id))]
             (conj acc
                   {"id"         entity-id
                    "type"       "HydrogenElectrolysisCase"
                    "labelEn"    name-val
                    "confidence" "0.95"
                    "license"    "CC0-1.0"
                    "sourceId"   "hydrogen_electrolysis actor"
                    "claims"     [(claim "case-name" name-val)
                                  (claim "actor"
                                         (get row ":hydrogen.electrolysis/actor" ""))
                                  (claim "engine"
                                         (get row ":hydrogen.electrolysis/engine" ""))
                                  (claim "electrical-kwh-per-kg-h2"
                                         (get row ":hydrogen.electrolysis/electrical-kwh-per-kg-h2" ""))
                                  (claim "total-with-heat-kwh-per-kg-h2"
                                         (get row ":hydrogen.electrolysis/total-with-heat-kwh-per-kg-h2" ""))
                                  (claim "hhv-electrical-efficiency-pct"
                                         (get row ":hydrogen.electrolysis/hhv-electrical-efficiency-pct" ""))
                                  (claim "hhv-total-efficiency-pct"
                                         (get row ":hydrogen.electrolysis/hhv-total-efficiency-pct" ""))
                                  (claim "h2-kg-per-hour"
                                         (get row ":hydrogen.electrolysis/h2-kg-per-hour" ""))
                                  (claim "output-pressure-bar"
                                         (get row ":hydrogen.electrolysis/output-pressure-bar" ""))]
                    "relations"  []}))))))
   []
   datoms))
