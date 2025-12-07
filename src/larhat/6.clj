(ns larhat.6
  (:require
   [larhat.prelude :refer :all]
   [clojure.string :as str]))


(defn non-empty-words [s]
  (into [] (filter not-empty) (words s)))


(defn ->problems [data]
  (mapv non-empty-words data))


(defn transpose [m]
  (apply mapv vector m))


(def ACTIONS {"+" +
              "*" *
              \+ +
              \* *})

(defn compute [problem]
  (let [r-op (last problem)
        op (ACTIONS r-op)
        r-args (subvec problem 0 (dec (count problem)))
        args (mapv parse-int r-args)]
    (reduce op args)))


(defn solve-1 [data]
  (->> data
    (->problems)
    transpose
    (mapv compute)
    (reduce +)))

(defn compute-2 [op args]
  (reduce op args))

(defn solve-2 [data]
  (let [p1 (map reverse data)
        p2 (butlast p1)
        tp (transpose p2)
        spaced-args (mapv #(apply str %) tp)
        raw-args (mapv str/trim spaced-args)
        grouped-args (partition-by empty? raw-args)
        cleaned-args (remove #(= 1 (count %)) grouped-args)
        args (map #(map parse-int %) cleaned-args)
        spaced-ops (last p1)
        nil-ops (map ACTIONS spaced-ops)
        ops (remove nil? nil-ops)
        answers (map compute-2 ops args)]
    (reduce + answers)))


(defn day-6-1 [data]
  (solve-1 data))


(defn run-day-6-1 []
  (day-6-1 (inp-lines 6)))


(defn day-6-2 [data]
  (solve-2 data))


(defn run-day-6-2 []
  (day-6-2 (inp-lines 6)))
