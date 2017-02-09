(ns looping-is-recursion)

(defn power [base exp]
   (let [helper (fn [acc base exp]
                  (if (= exp 0)
                      acc
                      (recur (* acc base) base (dec exp))
                  )
                )
        ]
      (helper 1 base exp)
   )
)

(defn power_cl [base exp]
   (if (= exp 0)
       1
       (* base (power_cl base (dec exp)))
   )
)

(defn last-element [a-seq]
   (let [helper (fn [last a-seq]
                   (if (empty? a-seq)
                        last
                        (recur (first a-seq) (rest a-seq))
                   )
                 )
         ]
     (helper nil a-seq)
   )
)


(defn seq= [seq1 seq2]
   (let [helper (fn [seq1 seq2]
                    (cond
                       (and (empty? seq1) (empty? seq2))
                          true
                       (or (empty? seq1) (empty? seq2))     ;xor
                          false
                       (not (= (first seq1) (first seq2)))
                          false
                       :else
                          (recur (rest seq1) (rest seq2))
                    )
                )
         ]
       (helper seq1 seq2)
    )
)




(defn find-first-index [pred a-seq]
   (loop [i 0
          seq a-seq]
     (cond
         (empty? seq)
           nil
         (pred (first seq))   ; pred?
           i
         :else
           (recur (inc i) (rest seq))
     )
  )
)

(defn avg [a-seq]
   (loop [n 0
          sum 0
          seq a-seq]
      (cond
         (empty? seq)
           (/ sum n)
         :else
           (recur (inc n) (+ sum (first seq)) (rest seq))
      )
   )
)

(defn parity [a-seq]
   (loop [odds #{}
          seq a-seq]
      (cond
          (empty? seq)
             odds
          (nil? (odds (first seq)))
             (recur (conj odds (first seq)) (rest seq))
          :else
             (recur (disj odds (first seq)) (rest seq))
      )
   )
)


(defn fast-fibo [n]
  (loop [ f1   0
          f2   1
          i    0]
     (if (= i n)
         f1
         (recur f2 (+ f1 f2) (inc i))
     )
  )
)



(defn cut-at-repetition [a-seq]
  (loop [elementos #{}
         sequ a-seq
         init []]
     (cond
        (empty? sequ)
           a-seq
        (not (nil? (elementos (first sequ))))
           init
        :else
           (recur (conj elementos (first sequ)) (rest sequ) (conj init (first sequ)))
     )
  )
)

