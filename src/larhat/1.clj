(ns larhat.1
  (:require
   [larhat.prelude :refer :all]
   [clojure.string :as str]
   ))


(defn lr [s]
  (-> s
    (str/replace "L" "-")
    (str/replace "R" "")))


(defn rot+ [a b]
  (mod (+ a b) 100))


(defn pt-1 [& args]
  (let [[[s c] x] args]
    (let [s' (rot+ s (or x 0))]
      [s' (if (zero? s') (inc c) c)])))


(defn solve [f data]
  (let
      [xf (comp
            (map lr)
            (map parse-int))]
    (->> data
      (cons 50)
      (transduce xf f [0 0])
      (second))))


(defn day-1-1 [data]
  (solve pt-1 data))


(defn run-day-1-1 []
  (day-1-1 (inp-lines 1)))


(defn hilo [a b]
  (if (>= b 0)
    ;; right
    (let [lo        (int (Math/ceil (/ (+ 1 a) 100)))
          hi        (int (Math/floor (/ (+ a b) 100)))]
      [hi lo])
    ;; left
    (let [abs-b     (Math/abs b)
          lo        (int (Math/ceil (/ (- a abs-b) 100)))
          hi        (int (Math/floor (/ (- a 1) 100)))]
      [hi lo])))


(defn crosses [a b]
  (let [[hi lo] (hilo a b)]
    (if (<= lo hi) (inc (- hi lo )) 0)))


(defn rot2+ [a b]
  (let [end (+ a b)
        pos (mod end 100)
        crossings (crosses a b)]
    [pos crossings]))


(defn pt-2 [& args]
  (let [[[s c] x] args]
    (let [[s' c'] (rot2+ s (or x 0))
          res [s' (+ c c')]
          ]
      (printf "pos=%s; rot=%s ==> newpos=%s; zeroes=%s => %s\n"
        s
        (or x 0)
        s'
        c
        (+ c c'))
      res)))


(defn day-1-2 [data]
  (solve pt-2 data))


(defn run-day-1-2 []
  (day-1-1 (inp-lines 1)))
