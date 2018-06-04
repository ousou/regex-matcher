(ns regex-matcher.core-test
  (:require [clojure.test :refer :all]
            [regex-matcher.core :refer :all]))

  (def states-1 #{"S0" "S1"})
  (def transition-func-1 {{:state "S0", :char \a} "S1", {:state "S1", :char \a} "S0"})
  (def accept-states-1 #{"S1"})
  (def starting-state-1 "S0")

  (def odd-a (->dfa states-1 transition-func-1 accept-states-1 starting-state-1))

  (deftest accept-odd-amount-of-a
    (testing "Accepts single a")
      (is (= true (accept? odd-a "a")))
    (testing "Accepts five a")
      (is (= true (accept? odd-a "aaaaa")))
    (testing "Accepts 11 a")
      (is (= true (accept? odd-a (repeat 11 "a"))))
    (testing "Accepts 101 a")
      (is (= true (accept? odd-a (repeat 101 "a"))))
    (testing "Rejects zero a")
      (is (= false (accept? odd-a "")))
    (testing "Rejects two a")
      (is (= false (accept? odd-a "aa")))
    (testing "Rejects four a")
      (is (= false (accept? odd-a "aaaa")))
    (testing "Rejects 1000 a")
      (is (= false (accept? odd-a (repeat 1000 "a")))))
