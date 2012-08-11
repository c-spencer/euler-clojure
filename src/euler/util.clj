(ns euler.util
  (use (clojure.math numeric-tower combinatorics)))

(defn ^boolean fast-even? [^long n]
  (zero? (bit-and (clojure.lang.RT/uncheckedLongCast n) 1)))

(defn find-factor [n]
  (if
    (zero? n) 0
    (first (filter #(= (mod n %) 0)
      (lazy-cat
        [2]
        (range 3 (+ (Math/sqrt n) 1) 2)
        [n])))))

(defn- -prime-factors [r n]
  (if (<= n 1)
    r
    (let [factor (find-factor n)]
      (recur (conj r factor) (/ n factor)))))
(defn prime-factors [n] (-prime-factors '() n)) ; tail recursive version

(defn fibonacci []
  ((fn rfib [a b]
    (lazy-seq (cons a (rfib b (+' a b))))
  ) 0 1))

(defn palindrome? [n]
  (let [s (seq (str n))]
    (= s (reverse s))))

(defn least-common-multiple [& xs]
  (reduce *
    (map
      (fn [[k v]] (expt k v))
      (apply merge-with
        #(if (> %1 %2) %1 %2)
        (map (comp frequencies prime-factors) xs)))))

(defn ^boolean prime? [^long n]
  (cond (< n 2) false
        (= n 2) true
        (fast-even? n) false
        :else (let [limit (sqrt n)]
                (loop [i (int 3)]
                  (if (> i limit)
                    true
                    (if (= (rem n i) 0)
                      false
                      (recur (+ 2 i))))))))

(defn ^int parse-int [s] (Integer/parseInt s))
(defn ^long parse-long [s] (Long/parseLong s))

(defn prime-sieve [^long n]
  (let [^ints arr (int-array (inc n))
        root (int (sqrt n))]
    (loop [curr (int 3)
           primes (list 2)]
      (if (> curr n)
        (reverse primes)
        (if (zero? (aget arr curr))
          (do
            (when (<= curr root)
              (loop [idx (int curr)]
                (when (< idx n)
                  (aset arr idx 1)
                  (recur (+ curr idx)))))
            (recur (+ curr 2) (cons curr primes)))
          (recur (+ curr 2) primes))))))

(defn triangle [^long n] (/ (* n (inc n)) 2))
(defn triangles [] (map triangle (range)))

(defn number-of-divisors [^long n]
  (reduce (fn [r [k v]] (* r (inc v))) (int 1) (frequencies (prime-factors n))))

(defn factorial [n] (reduce * (range 1N (inc n))))

(defn binomial-choose [n k]
  (/ (factorial n)
     (* (factorial k)) (factorial (- n k))))

(defn proper-divisors [^long n]
  (filter
    #(not= % n)
    (distinct
      (map #(reduce * %) (subsets (prime-factors n))))))

(def proper-divisor-sum (memoize
  (fn [x] (reduce + (proper-divisors x)))))

(defn amicable? [n]
  (let [i (proper-divisor-sum n)]
    (and (not= i n)
         (= (proper-divisor-sum i) n))))

(defn multiplicative-order [g n]
  (loop [e 1]
    (cond
      (= e n) nil
      (= 1 (mod (expt g e) n)) e
      :else (recur (+ 1 e)))))

(defn decimal-repeat [n]
  (cond
    (= n 1) 0
    (= 0 (mod n 5)) (recur (/ n 5))
    (= 0 (mod n 2)) (recur (/ n 2))
    :else (multiplicative-order 10 n)))
