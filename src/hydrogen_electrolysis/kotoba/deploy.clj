(ns hydrogen-electrolysis.kotoba.deploy
  "Operator boundary for the electrolysis ingest. This repository can build a
  deterministic plan offline; live submission remains deliberately unavailable
  until an operator-authenticated Kotoba adapter is supplied."
  (:require [clojure.pprint :as pprint]
            [hydrogen-electrolysis.kotoba.ingest-efficiency :as ingest]
            [hydrogen-electrolysis.methods.analyze :as analyze]
            [hydrogen-electrolysis.methods.electrolysis :as electrolysis]))

(defn plan []
  (let [comparison (analyze/run-comparison-stub)
        datoms (electrolysis/kotoba-datoms comparison)
        entities (ingest/entities datoms)]
    {:actor "hydrogen_electrolysis"
     :graph "com.etzhayyim.hydrogen-electrolysis"
     :mode :dry-run
     :operator-required true
     :datom-count (count datoms)
     :entity-count (count entities)
     :entities entities}))

(defn -main [& args]
  (when (some #{"--live"} args)
    (throw (ex-info "live ingest requires an operator-authenticated Kotoba adapter"
                    {:status :refused :mode :live :operator-required true})))
  (pprint/pprint (plan)))
