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

  (def states-2 #{"S0" "S1" "S2" "S3" "S4" "S5" "SFail"})
  (def transition-func-2 {{:state "S0", :char \a} "S1",
                          {:state "S1", :char \b} "S2",
                          {:state "S2", :char \b} "S3",
                          {:state "S3", :char \a} "S4",
                          {:state "S4", :char \b} "S5",
                          {:state "S0", :char \b} "SFail",
                          {:state "S1", :char \a} "SFail",
                          {:state "S2", :char \a} "SFail",
                          {:state "S3", :char \b} "SFail",
                          {:state "S4", :char \a} "SFail",
                          {:state "S5", :char \a} "SFail",
                          {:state "S5", :char \b} "SFail",
                          {:state "SFail", :char \a} "SFail",
                          {:state "SFail", :char \b} "SFail"})
  (def accept-states-2 #{"S5"})
  (def starting-state-2 "S0")

  (def abbab (->dfa states-2 transition-func-2 accept-states-2 starting-state-2))

  (deftest accept-abbab
    (testing "Accepts abbbab")
      (is (= true (accept? abbab "abbab")))
    (testing "Rejects empty")
      (is (= false (accept? abbab "")))
    (testing "Rejects a")
      (is (= false (accept? abbab "a")))
    (testing "Rejects b")
      (is (= false (accept? abbab "b")))
    (testing "Rejects five a")
      (is (= false (accept? abbab "aaaaa")))
    (testing "Rejects ab")
      (is (= false (accept? abbab "ab")))
    (testing "Rejects abab")
      (is (= false (accept? abbab "abab")))
    (testing "Rejects 10 ab")
      (is (= false (accept? abbab (repeat 1000 "ab")))))
