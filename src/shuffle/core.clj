(ns shuffle.core
  (:gen-class))

(def cards-52
  (for [suit ["S" "H" "D" "C"]
        card [2 3 4 5 6 7 8 9 "T" "J" "Q" "K" "A"]]
    (str card suit)))

(def items-4 [1 2 3 4])

(defn naive-shuffle
  "This is an example of what not to do -- use Fisher-Yates instead."
  [cards-to-shuffle]
  (let [n (count cards-to-shuffle)]
    (loop [cards (vec cards-to-shuffle)
           i     0] ;; for every position
      (if (= i n)
        cards
        (let [j (rand-int n)]
          ;(prn "swapping positions" i j)
          (recur
           (assoc cards             ;; swap i and a random j
                  i (get cards j)
                  j (get cards i))
           (inc i)))))))


(naive-shuffle items-4)

(naive-shuffle cards-52)

(defn demo
  "Print how often an item is is 0th position"
  [shuffle-f items x]
  (println (into (sorted-map)
                 (frequencies (for [_ (range x)]
                                (get (shuffle-f items) 0))))))


;; DEMO badness
(println "=============")
(doseq [_ (range 1)]
  (demo naive-shuffle items-4 1000))
(doseq [_ (range 1)]
  (demo naive-shuffle cards-52 5200))

(defn good-shuffle ;; Fisher Yates as per Knuth
  [cards-to-shuffle]
  (let [cards-to-shuffle (vec cards-to-shuffle) ;; convert to vector for get
        n                (count cards-to-shuffle)]
    (loop [i     (dec n)
           cards cards-to-shuffle]
      (let [j (rand-int (inc i))] ;; 0 ≤ j ≤ i
        (if (= i 0)
          cards
          (recur (dec i)
                 (assoc cards
                        i (get cards j) ;; swap i,j positions
                        j (get cards i))))))))


;; use this (java implementation)
(shuffle cards-52)

(good-shuffle cards-52)

(good-shuffle items-4)

;; DEMO goodness
(doseq [i (range 1)]
  (demo shuffle items-4 1000))
(doseq [i (range 10)]
  (demo good-shuffle items-4 1000))
(doseq [i (range 1)]
  (demo good-shuffle cards-52 52000))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
