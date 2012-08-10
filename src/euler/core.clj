(ns euler.core
  (use euler.util)
  (use [euler.defs :as defs])
  (use clojure.math.numeric-tower)
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
