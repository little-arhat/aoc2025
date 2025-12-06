(ns larhat.3
  (:require
   [larhat.prelude :refer :all]
   [clojure.string :as str]
   ))



(defn max-two-digit-number [s]
  (let [d (digits s)
        ed (enumerate d)
        without-last (drop-last ed)
        rv (reverse without-last)
        [idx max-first-digit] (apply max-key last rv)
        post-chosen (drop-while #(<= (first %) idx) ed)
        [_ max-snd-digit] (apply max-key last post-chosen)]
    (+ (* 10 max-first-digit) max-snd-digit)))


(defn max-twelve-digit-number [s]
  (let [k 12
        digits (mapv #(Character/digit % 10) s)
        n      (count digits)]
    (loop [i     0
           stack []]
      (if (= i n)
        (parse-int (apply str (take k stack)))
        (let [d (digits i)]
          (recur (inc i)
            (loop [st stack]
              (if (and (seq st)
                    (> d (peek st))
                    (> (- n i) (- k (count st))))
                (recur (pop st))
                (if (< (count st) k)
                  (conj st d)
                  st)))))))))


(defn solve [f data]
  (->> data
    (map f)
    (reduce +)))


(defn day-3-1 [data]
  (solve max-two-digit-number data))


(defn run-day-3-1 []
  (day-3-1 (inp-lines 3)))


(defn day-3-2 [data]
  (solve max-twelve-digit-number data))


(defn run-day-3-2 []
  (day-3-2 (inp-lines 3)))
