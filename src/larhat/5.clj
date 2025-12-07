(ns larhat.5
  (:require
   [larhat.prelude :refer :all]
   [clojure.string :as str]))


(defn overlap? [prev-to from]
  (and (some? prev-to) (<= from prev-to)))


(defn merge-range [merged new-range]
  (let [[from to] new-range
        [prev-from prev-to] (last merged)]
    (if (overlap? prev-to from)
      (conj (vec (butlast merged)) [prev-from (max to prev-to)])
      (conj merged [from to]))))


(defn parse-range [string-pair]
  (map parse-int (str/split string-pair #"-")))


(defn mk-fresh [ingridient]
  (fn [[from to]]
    (and (<= from ingridient) (<= ingridient to))))


(defn fresh? [fresh-ranges ingridient]
  (some (mk-fresh ingridient) fresh-ranges))


(defn fresh-in-range [[from to]]
  (inc (- to from)))


(defn solve-1 [[fresh-ranges ingridients]]
  (let [ing-ids (map parse-int (lines ingridients))
        pairs (map parse-range (lines fresh-ranges))
        sp (sort #(compare (first %1) (first %2)) pairs)
        ranges (reduce merge-range [] sp)
        check-fresh (partial fresh? ranges)]
    (count (filter check-fresh ing-ids))))


(defn solve-2 [[fresh-ranges _]]
  (->> fresh-ranges
    lines
    (map parse-range)
    (sort #(compare (first %1) (first %2)))
    (reduce merge-range [])
    (map fresh-in-range)
    (reduce +)))


(defn day-5-1 [data]
  (solve-1 data))


(defn run-day-5-1 []
  (day-5-1 (inp-phrases 5)))


(defn day-5-2 [data]
  (solve-2 data))


(defn run-day-5-2 []
  (day-5-2 (inp-phrases 5)))
