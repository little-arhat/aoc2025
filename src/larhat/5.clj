(ns larhat.5
  (:require
   [larhat.prelude :refer :all]
   [clojure.string :as str]))


(defn overlaps? [prev-to from]
  (and (some? prev-to) (<= from prev-to)))


(defn merge-range [merged [from to]]
  (if-let [[prev-from prev-to] (peek merged)]
    (if (overlaps? prev-to from)
      (conj (pop merged) [prev-from (max to prev-to)])
      (conj merged [from to]))
    (conj merged [from to])))


(defn parse-int-range [string-pair]
  (parse-range string-pair #"-" parse-int))


(defn in-range? [ingredient [from to]]
  (and (<= from ingredient) (<= ingredient to)))


(defn solve-1 [[fresh-ranges ingridients]]
  (let [pairs (->> fresh-ranges
                lines
                (map parse-int-range)
                (sort-by first))
        ranges (reduce merge-range [] pairs)
        fresh? #(some (partial in-range? %) ranges)
        ing-ids (lines ingridients)
        xf (comp
             (map parse-int)
             (filter fresh?))]
    (transduce xf
      (completing (fn [acc _] (inc acc)))
      0
      ing-ids)))


(defn fresh-in-range [[from to]]
  (inc (- to from)))


(defn solve-2 [[fresh-ranges _]]
  (->> fresh-ranges
    lines
    (map parse-int-range)
    (sort-by first)
    (reduce merge-range [])
    (transduce (map fresh-in-range) + 0)))


(defn day-5-1 [data]
  (solve-1 data))


(defn run-day-5-1 []
  (day-5-1 (inp-phrases 5)))


(defn day-5-2 [data]
  (solve-2 data))


(defn run-day-5-2 []
  (day-5-2 (inp-phrases 5)))
