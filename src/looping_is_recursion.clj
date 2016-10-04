(ns looping-is-recursion)

(defn power-helper [acc base exp]
  (if (== 0 exp) acc (power-helper (* acc base) base (dec exp)))
)

(defn power [base exp]
 (power-helper 1 base exp)
)

(defn last-element-helper [acc a-seq]
  (if (< (count a-seq) 1) acc (last-element-helper (first a-seq) (rest a-seq)))
)

(defn last-element [a-seq]
(last-element-helper nil a-seq)
)

(defn seq=-helper [acc seq1 seq2]
 (cond
   (and (empty? seq1) (empty? seq2)) acc
   (or (empty? seq1) (empty? seq2)) false
   (not= (first seq1) (first seq2)) false
   :else (seq=-helper true (rest seq1) (rest seq2)))
)


(defn seq= [seq1 seq2]
 (seq=-helper true seq1 seq2)
)

(defn find-first-index [pred a-seq]
 (loop [p 0
        f (first a-seq)
        r (rest a-seq)]
  (cond
    (nil? f) nil
    (pred f) p
    :else (recur (+ p 1) (first r) (rest r))))
)

(defn avg [a-seq]
  (loop [coll a-seq
        currsum 0
         ind 0]
    (cond
      (== (count coll) ind) (/ currsum ind)
      (== (count coll) 0) 0
      :else (recur coll (+ currsum (get coll ind)) (inc ind))))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
(loop [b-seq a-seq my-set (set [])]
    (if (empty? b-seq)
      my-set
      (recur (rest b-seq) (toggle my-set (first b-seq))))))


(defn fast-fibo [n]
(loop [n n previous 0 current 1]
(cond
(zero? n) previous
:else (recur (dec n) current (+ current previous)))))


(defn add-element [element v]
(if (contains? v element) v (conj v element))
)

(defn cut-at-reptition [a-seq]
(loop [state true oldseq a-seq newseq [] ]
  (cond
   (empty? oldseq) newseq
   (= state false) newseq
   :else (recur  (if (contains? newseq (first oldseq)) false true) (rest oldseq) (add-element (first oldseq) (newseq))))
)
)

 (defn cut-at-repetition [a-seq]
(loop [b-seq a-seq my-seq []]
(cond
(empty? b-seq) my-seq
(some #{(first b-seq)} my-seq) my-seq
:else (recur (rest b-seq) (conj my-seq (first b-seq))))))
