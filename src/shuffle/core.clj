(ns shuffle.core
  (:gen-class))

(def cards-52
  (for [suit ["S" "H" "D" "C"]
        card [2 3 4 5 6 7 8 9 "T" "J" "Q" "K" "A"]]
    (str card suit)))

(def items-3 [1 2 3])
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
  (println
   (map (fn [[k v]] (format "%s occurs %d, (%2.1f%%)\n" k v (* 100.0 (/ v x))))
        (into (sorted-map)
              (frequencies (for [_ (range x)]
                             (get (shuffle-f items) 0)))))))


;; DEMO badness
(println "=============")
(doseq [_ (range 1)]
  (demo naive-shuffle items-3 1000000))
(doseq [_ (range 1)]
  (demo naive-shuffle cards-52 5200))

;; But why is it bad? Naive shuffle produces "n to the power n" non distinct
;; arrangements with equal probability e.g. for 3 items this is 27 arrangements.

;; The actual number of distinct arrangements of 3 items is 3! = 6, but
;; 27 / 6 = 4 rem 3. i.e. no matter how the arrangements fall into the 6 distinct
;; arrangements, it is not possible for these to form a probablity of 1/6, because
;; 6 is not a factor of 27.
;;
;; In this case,
;; [1, 2, 3] occurs 4/27 times != 1/6
;; [3, 1, 2] occurs 4/27 times != 1/6
;; [3, 2, 1] occurs 4/27 times != 1/6
;; [2, 1, 3] occurs 5/27 times != 1/6
;; [2, 3, 1] occurs 5/27 times != 1/6
;; [1, 3, 2] occurs 5/27 times != 1/6


(defn good-shuffle ;; Fisher Yates as per Knuth
  [cards-to-shuffle]
  (let [cards-to-shuffle (vec cards-to-shuffle) ;; convert to vector for get
        n                (count cards-to-shuffle)]
    (loop [i     (dec n)
           cards cards-to-shuffle]    ;; 1 ≤ i ≤ n-1
      (if (= i 0)
        cards
        (let [j (rand-int (inc i))]   ;; 0 ≤ j ≤ i
          (recur (dec i)
                 (assoc cards
                        i (get cards j) ;; swap i,j positions
                        j (get cards i))))))))

;; Why does it work? => It can produce n! arrangements with equal probablity.
;; These correspond to the n! distinct permutations.

;; use this (java implementation)
(shuffle cards-52)

;; or this
(good-shuffle cards-52)

(good-shuffle items-3)

;; DEMO goodness
(doseq [i (range 1)]
  (demo shuffle items-4 1000))
(doseq [i (range 10)]
  (demo good-shuffle items-4 1000))
(doseq [i (range 1)]
  (demo good-shuffle cards-52 5200))

(defn -main
  [& args])
