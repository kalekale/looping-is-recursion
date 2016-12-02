(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp acc]
                (if (zero? exp)
                  acc
                  (recur base (dec exp) (* acc base))))]
    (helper base exp 1)))


(defn last-element [a-seq]
  (let [helper (fn [a-seq acc]
                 (if (zero? (count a-seq))
                   acc
                   (recur (rest a-seq) (first a-seq))))]
    (helper a-seq (first a-seq))))



(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2 isEqual]
                 (if (and (empty? s1) (empty s2))
                   true
                 (if (or (not= (count s1) (count s2))
                         (not isEqual))
                   false
                   (recur (rest s1) (rest s2) (= (first s1) (first s2))))))]
    (helper seq1 seq2 (= (first seq1) (first seq2)))))


(defn find-first-index [pred a-seq]
  (loop [n 0
         p pred
         s a-seq]
    (if (empty? s)
      nil
      (if (p (first s))
        n
        (recur (inc n) p (rest s))))))
             ;=> 3

(defn avg [a-seq]
  (loop [s a-seq
         sum 0
         howmany (count a-seq)]
    (if (empty? s)
      (/ sum howmany)
      (recur (rest s) (+ sum (first s)) howmany))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [s a-seq
         a #{}]
    (if (empty? s)
      a
      (recur (rest s) (toggle a (first s))))))


(defn fast-fibo [n]
  (loop [x 1
         x-1 0
         n n]
    (if (zero? n)
      x-1
      (recur (+ x x-1) x (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [s a-seq
         found (set [])
         return (vec [])]
    (if (empty? s)
      return
      (if (contains? found (first s))
        return
        (recur (rest s) (conj found (first s)) (conj return (first s)))))))





















