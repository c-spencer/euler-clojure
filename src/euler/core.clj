(ns euler.core
  (use euler.util)
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
  (let [big-number 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450]
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
  (let [grid [ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8
              49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0
              81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65
              52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91
              22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
              24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50
              32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
              67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21
              24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
              21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95
              78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92
              16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57
              86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58
              19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40
               4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66
              88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69
               4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36
              20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16
              20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54
               1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48]]
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
