(ns euler.core-test
  (:use midje.sweet
        euler.core))

(fact "euler-problems"
  (euler-problem 1) => 233168
  (euler-problem 2) => 4613732
  (euler-problem 3) => 6857
  (euler-problem 4) => 906609
  (euler-problem 5) => 232792560
  (euler-problem 6) => 25164150
  (euler-problem 7) => 104743
  (euler-problem 8) => 40824
  (euler-problem 9) => 31875000
  (euler-problem 10) => 142913828922
  (euler-problem 11) => 70600674
  (euler-problem 12) => 76576500
  (euler-problem 13) => 5537376230
  (euler-problem 14) => 837799
  (euler-problem 15) => 137846528820
  (euler-problem 16) => 1366
  (euler-problem 17) => 21124
  (euler-problem 18) => 1074
  (euler-problem 19) => 171
  (euler-problem 20) => 648
  (euler-problem 21) => 31626
  (euler-problem 22) => 871198282
  (euler-problem 23) => 4179871
  (euler-problem 24) => 2783915460
  (euler-problem 25) => 4782
  (euler-problem 26) => 983
  (euler-problem 27) => -59231
  (euler-problem 28) => 669171001
  (euler-problem 29) => 9183
  (euler-problem 30) => 443839)
