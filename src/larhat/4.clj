(ns larhat.4
  (:require
   [larhat.prelude :refer :all]
   [clojure.string :as str]))


(defn roll? [x]
  (= x \@))


(defn free-roll? [{:keys [element neighbours]}]
  (when (roll? element)
    (->> neighbours
      (filter roll?)
      (count)
      (#(< % 4)))))


(defn ->map [data]
  (->grid identity data))


(defn solve-1 [data]
  (->> data
    (->map)
    (map-grid-with-adj free-roll?)
    (concat* )
    (filter some?)
    (count)))


(defn remove-free [{:keys [element] :as o}]
  (if (free-roll? o)
    \.
    element))


(defn remove-free* [grid]
  (map-grid-with-adj remove-free grid))


(defn count-rolls [grid]
  (->> grid
    (concat*)
    (filter roll?)
    (count)))


(defn solve-2 [data]
  (let [prob (->map data)
        cleared (fixpoint remove-free* prob)
        orig-rolls (count-rolls prob)
        new-rolls (count-rolls cleared)]
    (- orig-rolls new-rolls)))


(defn day-4-1 [data]
  (solve-1 data))


(defn run-day-4-1 []
  (day-4-1 (input 4)))


(defn day-4-2 [data]
  (solve-2 data))


(defn run-day-4-2 []
  (day-4-2 (input 4)))
