(ns regex-matcher.core-test
  (:require [clojure.test :refer :all]
            [regex-matcher.core :refer :all]))

  (def states-1 #{"S0" "S1"})
  (def transition-func-1 {{:state "S0", :char \a} "S1", {:state "S1", :char \a} "S0"})
  (def accept-states-1 #{"S1"})
  (def starting-state-1 "S0")

  (def odd-a (->dfa states-1 transition-func-1 accept-states-1 starting-state-1))

  (deftest dfa-accept-odd-amount-of-a
    (testing "Accepts single a")
      (is (= true (accept-dfa? odd-a "a")))
    (testing "Accepts five a")
      (is (= true (accept-dfa? odd-a "aaaaa")))
    (testing "Accepts 11 a")
      (is (= true (accept-dfa? odd-a (repeat 11 "a"))))
    (testing "Accepts 101 a")
      (is (= true (accept-dfa? odd-a (repeat 101 "a"))))
    (testing "Rejects zero a")
      (is (= false (accept-dfa? odd-a "")))
    (testing "Rejects two a")
      (is (= false (accept-dfa? odd-a "aa")))
    (testing "Rejects four a")
      (is (= false (accept-dfa? odd-a "aaaa")))
    (testing "Rejects 1000 a")
      (is (= false (accept-dfa? odd-a (repeat 1000 "a")))))

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

  (deftest dfa-accept-abbab
    (testing "Accepts abbbab")
      (is (= true (accept-dfa? abbab "abbab")))
    (testing "Rejects empty")
      (is (= false (accept-dfa? abbab "")))
    (testing "Rejects a")
      (is (= false (accept-dfa? abbab "a")))
    (testing "Rejects b")
      (is (= false (accept-dfa? abbab "b")))
    (testing "Rejects five a")
      (is (= false (accept-dfa? abbab "aaaaa")))
    (testing "Rejects ab")
      (is (= false (accept-dfa? abbab "ab")))
    (testing "Rejects abab")
      (is (= false (accept-dfa? abbab "abab")))
    (testing "Rejects 10 ab")
      (is (= false (accept-dfa? abbab (repeat 1000 "ab")))))

  (def states-nfa-1 #{"S0" "S1" "S2" "S3" "S4" "S5"})
  (def transition-func-nfa-1 {{:state "S0", :char \a} #{"S1"},
                          {:state "S1", :char \b} #{"S2"},
                          {:state "S2", :char \b} #{"S3"},
                          {:state "S3", :char \a} #{"S4"},
                          {:state "S4", :char \b} #{"S5"}})
  (def accept-states-nfa-1 #{"S5"})
  (def starting-state-nfa-1 "S0")

  (def abbab-nfa (->nfa states-nfa-1 transition-func-nfa-1 accept-states-nfa-1 starting-state-nfa-1))

  (deftest nfa-accept-abbab
    (testing "Accepts abbab")
      (is (= true (accept-nfa? abbab-nfa "abbab")))
    (testing "Rejects empty")
      (is (= false (accept-nfa? abbab-nfa "")))
    (testing "Rejects a")
      (is (= false (accept-nfa? abbab-nfa "a")))
    (testing "Rejects b")
      (is (= false (accept-nfa? abbab-nfa "b")))
    (testing "Rejects five a")
      (is (= false (accept-nfa? abbab-nfa "aaaaa")))
    (testing "Rejects ab")
      (is (= false (accept-nfa? abbab-nfa "ab")))
    (testing "Rejects abab")
      (is (= false (accept-nfa? abbab-nfa "abab")))
    (testing "Rejects 1000 ab")
      (is (= false (accept-nfa? abbab-nfa (repeat 1000 "ab")))))

  (def states-nfa-2 #{"S0" "S1" "S2"})
  (def transition-func-nfa-2 {{:state "S0", :char \a} #{"S1", "S2"},
                          {:state "S1", :char \b} #{"S0"}})
  (def accept-states-nfa-2 #{"S2"})
  (def starting-state-nfa-2 "S0")

  (def abstar-then-a-nfa (->nfa states-nfa-2 transition-func-nfa-2 accept-states-nfa-2 starting-state-nfa-2))

  (deftest nfa-accept-abstar-a
    (testing "Accepts a")
      (is (= true (accept-nfa? abstar-then-a-nfa "a")))
    (testing "Rejects empty")
      (is (= false (accept-nfa? abstar-then-a-nfa "")))
    (testing "Rejects ab")
      (is (= false (accept-nfa? abstar-then-a-nfa "ab")))
    (testing "Accepts aba")
      (is (= true (accept-nfa? abstar-then-a-nfa "aba")))
    (testing "Rejects five a")
      (is (= false (accept-nfa? abstar-then-a-nfa "aaaaa")))
    (testing "Rejects aab")
      (is (= false (accept-nfa? abstar-then-a-nfa "aab")))
    (testing "Rejects abba")
      (is (= false (accept-nfa? abstar-then-a-nfa "abba")))
    (testing "Accepts ababa")
      (is (= true (accept-nfa? abstar-then-a-nfa "ababa")))
    (testing "Accepts ababababababababababa")
      (is (= true (accept-nfa? abstar-then-a-nfa "ababababababababababa")))
    (testing "Accepts 1000 ab then a")
      (is (= true (accept-nfa? abstar-then-a-nfa (str (apply str (repeat 1000 "ab")) "a"))))
    (testing "Rejects 5000 ab")
      (is (= false (accept-nfa? abstar-then-a-nfa (apply str (repeat 5000 "ab"))))))
