(ns euler.util
  (use clojure.math.numeric-tower))

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
    (lazy-seq (cons a (rfib b (+ a b))))
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

(defn prime? [n]
  (if (< n 2)
    false
    (let [limit (sqrt n)]
      (loop [i 2]
        (if (> i limit)
          true
          (if (= (rem n i) 0)
            false
            (recur (inc i))))))))

(defn parse-int [s] (Integer/parseInt s))
(defn parse-long [s] (Long/parseLong s))

(defn prime-sieve [n]
  (let [arr (int-array (inc n))
        root ((comp inc int) (Math/floor (Math/sqrt n)))]
    (loop [curr 3
           primes (list 2)]
      (if (> curr n)
        (reverse primes)
        (if (zero? (aget arr curr))
          (do
            (if (< curr root)
              (dorun (for [idx (range curr n curr)] (aset arr idx 1))))
            (recur (+ curr 2) (conj primes curr)))
          (recur (+ curr 2) primes))))))

(defn triangle [^long n] (/ (* n (inc n)) 2))
(defn triangles [] (map triangle (range)))

(defn number-of-divisors [^long n]
  (reduce (fn [r [k v]] (* r (inc v))) (int 1) (frequencies (prime-factors n))))
