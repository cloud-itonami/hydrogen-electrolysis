(ns hydrogen-electrolysis.methods.test-analyze
  "Tests for analyze.cljc — the entry-point port of methods/analyze.py.
  Covers: run-comparison-stub shape + write-outputs! file emission.
  The kami-sim engine leg (run_comparison) is omitted in both Python and cljc;
  tests drive the pure stub."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            #?(:clj [clojure.java.io :as io])
            [hydrogen-electrolysis.methods.analyze :as a]
            [hydrogen-electrolysis.methods.electrolysis :as e]))

;; ---------------------------------------------------------------------------
;; run-comparison-stub
;; ---------------------------------------------------------------------------

(deftest test-stub-shape
  (let [c (a/run-comparison-stub)]
    (is (= "hydrogen_electrolysis" (get c "actor")))
    (is (= "kami-hydrogen-electrolysis-sim" (get c "engine")))
    (is (= 10000.0 (get c "active_area_cm2")))
    (is (= "cfe-zero-gap-aem-high-pressure" (get-in c ["best_low_temperature" "name"])))
    (is (= "soec-high-temperature" (get-in c ["best_electrical" "name"])))
    (is (= 2 (count (get c "results"))))))

(deftest test-stub-custom-area
  (let [c (a/run-comparison-stub 5000.0)]
    (is (= 5000.0 (get c "active_area_cm2")))))

(deftest test-stub-feeds-kotoba-datoms
  (let [c     (a/run-comparison-stub)
        datoms (e/kotoba-datoms c)]
    (is (pos? (count datoms)))
    (is (some #(= "cfe-zero-gap-aem-high-pressure"
                  (get % ":hydrogen.electrolysis/recommended-case"))
              datoms))))

(deftest test-stub-feeds-render-report
  (let [c      (a/run-comparison-stub)
        report (e/render-report c)]
    (is (str/includes? report "efficiency comparison"))
    (is (str/includes? report "10000 cm^2"))))

;; ---------------------------------------------------------------------------
;; write-outputs! (clj only — requires file system)
;; ---------------------------------------------------------------------------

#?(:clj
   (deftest test-write-outputs-creates-files
     (let [tmp-dir (doto (io/file (System/getProperty "java.io.tmpdir")
                                  (str "test-analyze-" (System/currentTimeMillis)))
                     .mkdirs)
           c       (a/run-comparison-stub)
           result  (a/write-outputs! c (str tmp-dir))]
       (try
         (is (= 3 (count (:files result))))
         (doseq [f-path (:files result)]
           (is (.exists (io/file f-path)) (str "missing: " f-path)))
         (let [comparison-data (edn/read-string (slurp (io/file tmp-dir "comparison.edn")))]
           (is (= "hydrogen_electrolysis" (get comparison-data "actor"))))
         ;; Markdown is a derived report, not the canonical data source.
         (let [md-txt (slurp (io/file tmp-dir "comparison-report.md"))]
           (is (str/includes? md-txt "efficiency comparison")))
         (let [datoms (edn/read-string (slurp (io/file tmp-dir "kotoba-datoms.edn")))]
           (is (= 3 (count datoms))))
         (finally
           (doseq [f (.listFiles tmp-dir)] (.delete f))
           (.delete tmp-dir))))))
