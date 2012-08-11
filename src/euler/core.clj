(ns euler.core
  (use euler.util)
  (use [euler.defs :as defs])
  (use (clojure.math numeric-tower combinatorics))
  (:refer-clojure :exclude [==])
  (use clojure.core.logic))

(defmulti euler-problem (fn [i]
  (do
    (println "Running euler problem" i)
    i)))

(defn solve-problems [f t]
  (doseq [n (range f t)]
    (println "Problem" n "=>" (euler-problem n))))

(defmethod euler-problem 1 [_]
  (reduce + (distinct (concat (range 3 1000 3) (range 5 1000 5)))))

(defmethod euler-problem 2 [_]
  (reduce +
    (take-while #(<= % 4000000)
      (filter even? (fibonacci)))))

(defmethod euler-problem 3 [_]
  (first (prime-factors 600851475143)))

(defmethod euler-problem 4 [_]
  (apply max
    (for [i (range 100 999)
          j (range 99 999 11)
          :when (palindrome? (* i j))]
      (* i j))))

(defmethod euler-problem 5 [_]
  (apply least-common-multiple (range 2 20)))

(defmethod euler-problem 6 [_]
  (let [square #(* % %)]
    (- (square (apply + (range 101)))
       (apply + (map square (range 101))))))

(defmethod euler-problem 7 [_]
  (nth (filter prime? (range)) 10000))

(defmethod euler-problem 8 [_]
  (let [big-number defs/problem8]
    (apply max
      (map
        #(reduce * %)
        (partition 5 1 (map (comp parse-int str) (str big-number)))))))

(defmethod euler-problem 9 [_]
  (let [limit 1000]
    (first
      (for [a (range 1 limit)
            b (range a (- limit a))
            c (list (- limit a b))
            :when (= (+ (* a a) (* b b)) (* c c))]
        (* a b c)))))

(defmethod euler-problem 10 [_]
  (reduce + (prime-sieve 2000000)))

(defmethod euler-problem 11 [_]
  (let [grid defs/problem11]
    (->>
      (run* [x y o]
        (infd x y (interval 0 19))
        (conde [(<fd y 17) (conde [(== o 20)]             ; vertical
                                  [(<fd x 17) (== o 21)]  ; diagonal-right
                                  [(<fd 2 x) (== o 19)])] ; diagonal-left
               [(<fd x 17) (== o 1)]))                    ; horizontal
      (map (fn [[x y o]] (->> (range (+ x (* y 20)) 400 o)
                         (take 4) (map grid) (reduce *))))
      (apply max))))

(defmethod euler-problem 12 [_]
  (->> (triangles)
       (filter #(> (number-of-divisors %) 500))
       (first)))

(defmethod euler-problem 13 [_]
  (->> (reduce + defs/problem13)
       (str)
       (take 10)
       (apply str)
       (parse-long)))

(defmethod euler-problem 14 [_]
  (loop [i (int 1)
         max-len (int 0)
         max-i (int 0)]
    (if (>= i 1000000)
      max-i
      (let [len (int (loop [j (int i)
                            n (int 1)]
                      (if (<= j 1)
                        n
                        (if (fast-even? j)
                          (recur (quot j 2) (+ 1 n))
                          (recur (inc (* j 3)) (+ 1 n))))))]
        (if (> len max-len)
          (recur (+ 2 i) len i)
          (recur (+ 2 i) max-len max-i))))))

(defmethod euler-problem 15 [_]
  (let [arr (make-array BigInteger 21 21)]
    (doseq [i (range 21)]
      (aset arr 0 i (biginteger 1))
      (aset arr i 0 (biginteger 1)))
    (doseq [x (range 1 21) y (range 1 21)]
      (aset arr x y (biginteger (+ (aget arr (dec x) y)
                                (aget arr x (dec y))))))
    (aget arr 20 20)))

(defmethod euler-problem 16 [_]
  (->> (expt 2 1000)
       (str)
       (map (comp parse-int str))
       (reduce +)))

(defmethod euler-problem 17 [_]
  (let [units ["one" "two" "three" "four" "five"
               "six" "seven" "eight" "nine"]
        tens  ["" "twenty" "thirty" "forty" "fifty"
               "sixty" "seventy" "eighty" "ninety"]
        tens-units (for [u (cons "" units) t tens] (str t u))
        teens ["ten" "eleven" "twelve" "thirteen"
               "fourteen" "fifteen" "sixteen" "seventeen"
               "eighteen" "nineteen"]
        below-hundred (concat tens-units teens)
        hundreds (for [u units b below-hundred]
          (if (zero? (count b))
            (str u "hundred")
            (str u "hundredand" b)))
        final (concat below-hundred hundreds ["onethousand"])]
    (reduce + (map count final))))

(defmethod euler-problem 18 [_]
  (let [sum-rows (fn [b t]
                   (map #(+ %1 (max %2 %3)) t b (rest b)))]
  (first (reduce sum-rows (reverse defs/problem18)))))
  
(defmethod euler-problem 19 [_]
  (let [leap-year? #(and (= 0 (mod % 4))
                         (or (not= 0 (mod % 100))
                             (= 0 (mod % 400))))
        year #(apply concat (map
                (partial range 1)
                [32 (if (leap-year? %) 30 29) 32 31 32
                 31 32 32 31 32 31 32]))]
    (->> (map
           #(vector %1 %2) ; start day of 1901 hard coded
           (cycle [:tue :wed :thur :fri :sat :sun :mon])
           (apply concat (map year (range 1901 2001))))
         (filter
           (fn [[d p]] (and (= :sun d) (= 1 p))))
         (count))))

(defmethod euler-problem 20 [_]
  (->> (factorial 100)
       (str)
       (map (comp parse-int str))
       (reduce +)))

(defmethod euler-problem 21 [_]
  (reduce + (filter amicable? (range 2 10000))))

(defmethod euler-problem 22 [_]
  (let [score (zipmap
                (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                (range 1 27))
        names (sort defs/problem22)]
    (reduce +
      (map * (for [name names]
               (reduce + (map score name)))
             (range 1 Double/POSITIVE_INFINITY)))))

(defmethod euler-problem 23 [_]
  (let [^ints arr (int-array 28123)
        ^ints abundants (int-array (filter #(> (proper-divisor-sum %) %) (range 1 28123)))]
    (loop [x (first abundants)
           xs (rest abundants)]
      (when (seq xs)
        (loop [y x
               ys xs]
          (when (and (< (+ x y) 28123) (seq ys))
            (aset arr (+ x y) 1)
            (recur (first ys) (rest ys))))
        (recur (first xs) (rest xs))))
    (loop [i (int 1)
           t 0]
      (if (= i 28123)
        t
        (if (= (aget arr i) 0)
          (recur (inc i) (+ t i))
          (recur (inc i) t))))))

(defmethod euler-problem 24 [_]
  (->> (nth (lex-permutations (range 10)) 999999)
       (apply str)
       (parse-long)))

(defmethod euler-problem 25 [_]
  (loop [a (bigint 1)
         b (bigint 1)
         c 1]
    (if (>= (count (str a)) 1000) c
      (recur b (+ a b) (+ 1 c)))))

(defmethod euler-problem 26 [_]
  (let [r (range 1 1000)
        lens (map decimal-repeat (range 1 1000))
        zipped (zipmap lens r)]
    (zipped (apply max lens))))

(defmethod euler-problem 27 [_]
  (let [prime-len (fn [^long a ^long b]
                    (loop [n (int 0)]
                      (if (prime? (+ (* n n) (* a n) b))
                        (recur (+ 1 n))
                        n)))
        s (range -999 1000)
        r (reduce
            (fn [r c]
              (if (> (c 2) (r 2)) c r))
            (for [a s b s]
              [a b (prime-len a b)]))]
    (* (r 0) (r 1))))

(defmethod euler-problem 28 [_]
  (loop [n 1
         s 2
         t 1]
    (if (>= n (* 1001 1001))
      t
      (recur (+ n (* 4 s)) (+ 2 s) (+ t (* 4 n) (* 10 s))))))

(defmethod euler-problem 29 [_]
  (let [r (range 2 101)]
    (count (distinct (for [a r b r] (expt a b))))))

(defmethod euler-problem 30 [_]
  (let [e5 (^long fn [^long n] (expt n 5))
        sum-pow-digit (^long fn [n] (e5 (parse-int (str n))))]
    (reduce
      (fn [t ^long x]
        (if (= x
              (->> x
                   (str)
                   (map sum-pow-digit)
                   (reduce +)))
          (+ x t)
          t))
      0
      (range 2 1000000))))
