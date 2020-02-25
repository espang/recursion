(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [current  (first a-seq)
          max-left (max-element (rest a-seq))]
      (if (nil? max-left)
        current
        (max current max-left)))))

(defn seq-max [seq-1 seq-2]
  (let [c-1 (count seq-1)
        c-2 (count seq-2)]
    (if (> c-1 c-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [current (first a-seq)
          longest (longest-sequence (rest a-seq))]
      (if (nil? longest)
        current
        (seq-max current longest)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [current (first a-seq)]
      (if (pred? current)
        (cons current (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (empty? a-seq) (empty? b-seq)
    (empty? b-seq) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) '()
    (empty? seq-2) '()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== 0 k)
    1
    (* n
       (power n (dec k)))))

(defn fib [n]
  (case n
    0 0
    1 1
    (+ (fib (dec n))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (seq (into #{} (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (update freqs (first a-seq) (fnil inc 0))
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[element freq] (first a-map)]
      (concat (repeat freq element) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (= 0 n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    '()
    (if (<= n 0)
      coll
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    [(my-take mid a-seq)
     (my-drop mid a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) (seq b-seq)
    (empty? b-seq) (seq a-seq)
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[l r] (halve a-seq)
          ls (merge-sort l)
          rs (merge-sort r)]
      (seq-merge ls rs))))

(defn- monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or (apply <= a-seq)
        (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (println a-seq)
  (if (empty? a-seq)
    '()
    (let [monotonic-sub-seqs (take-while monotonic? (inits a-seq))
          n (count monotonic-sub-seqs)]
      (concat (drop (dec n) monotonic-sub-seqs) (split-into-monotonics (drop (dec n) a-seq))))))

(split-into-monotonics [0 1 2 1 0])

(defn permutations [a-set]
  (cond
    (empty? a-set) '(())
    (singleton? a-set) (cons (seq a-set) '())
    :else   (let [a-set (set a-set)]
              (loop [s   a-set
                     ret '()]
                (if (empty? s)
                  ret
                  (let [current (first s)
                        perms   (permutations (disj a-set current))
                        append  (fn [coll] (cons current coll))]
                    (recur (rest s) (concat ret (map append perms)))))))))

(defn- combine [element subsets]
  (if (empty? subsets)
    #{#{} #{element}}
    (conj (combine element (rest subsets))
          (first subsets)
          (conj (first subsets) element))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (combine (first a-set) (powerset (rest a-set)))))

