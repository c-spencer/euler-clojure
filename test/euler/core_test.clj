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
  (euler-problem 12) => 76576500)
