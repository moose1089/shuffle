(ns shuffle.core
  (:gen-class))

(def cards-52
  (for [suit ["S" "H" "D" "C"]
        card [2 3 4 5 6 7 8 9 "T" "J" "Q" "K" "A"]]
    (str card suit)
 ))

(def items-4 [1 2 3 4])

(defn naive-shuffle
  "This is an example of what not to do -- use Fisher-Yates instead."
  [cards-to-shuffle]
  (let [n (count cards-to-shuffle)]
    (loop [cards (vec cards-to-shuffle)
           i     0] ;; for every position
      (if (= i n)
        cards
        (let [j (rand-int n)
              new-cards (assoc cards
                               i (get cards j)
                               j (get cards i))] ;; swap i and a random j
          ;(prn "swapping positions" i j)
          (recur
           new-cards
           (inc i)))))))


(naive-shuffle items-4)

(naive-shuffle cards-52)

(defn demo [shuffle-f items x]
  (println (into (sorted-map)
                 (frequencies (for [i (range x)]
                                (get (shuffle-f items) 0))))))

(println "=============")

(doseq [i (range 1)]
  (demo naive-shuffle items-4 1000))

(doseq [i (range 1)]
  (demo naive-shuffle cards-52 5200))


;; DEMO badness

(defn  good-shuffle
  [cards-to-shuffle])

;; use this
(shuffle cards-52)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
