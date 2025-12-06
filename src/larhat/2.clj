(ns larhat.2
  (:require
   [larhat.prelude :refer :all]
   [clojure.string :as str]
   ))


(defn repeated-many? [n]
  (let [s   (str n)
        len (count s)]
    (some #(when (zero? (mod len %))
             (= s (apply str (repeat (/ len %) (subs s 0 %)))))
          (range 1 (inc (quot len 2))))))


(defn repeated-twice? [n]
  (let [s   (str n)
        len (count s)]
    (when (even? len)
      (let [half (/ len 2)]
        (= (subs s 0 half)
           (subs s half))))))


(defn mk-gen-invalid [pred]
  (fn [[from to]]
    (filter pred (range (parse-int from) (inc (parse-int to))))))


(defn solve [pred data]
  (->> data
    (comma-sequence)
    (map #(str/split % #"-"))
    (map (mk-gen-invalid pred))
    (apply concat)
    (reduce +)))


(defn day-2-1 [data]
  (solve repeated-twice? data))

(defn run-day-2-1 []
  (day-2-1 (input 2)))


(defn day-2-2 [data]
  (solve repeated-many? data))

(defn run-day-2-2 []
  (day-2-2 (input 2)))
