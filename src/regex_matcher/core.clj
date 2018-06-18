(ns regex-matcher.core
  (:require [clojure.set :refer :all]))

(defrecord dfa [states transition-function accepting-states starting-state])

(defrecord nfa [states transition-function accepting-states starting-state])

(defn- next-state [dfa current-state char]
  (get (:transition-function dfa) {:state current-state :char char}))

(defn- is-accepting? [dfa current-state]
    (contains? (:accepting-states dfa) current-state))

(defn- accept-with-current-state [dfa string current-state]
  (if (empty? string)
    (is-accepting? dfa current-state)
    (accept-with-current-state dfa (apply str (rest string)) (next-state dfa current-state (first string)))))

(defn accept-dfa? [dfa string]
  (accept-with-current-state dfa (apply str string) (:starting-state dfa)))

(defn- next-states-nfa [nfa current-states char]
  (loop [nfa nfa
         current-states current-states
         char char
         next-states #{}]
     (if (empty? current-states)
       next-states
       (recur nfa (into #{} (rest current-states)) char (clojure.set/union next-states (next-state nfa (first current-states) char))))))

(defn- is-accepting-nfa? [nfa current-states]
    (not (empty? (clojure.set/intersection (:accepting-states nfa) current-states))))

(defn- do-eps-transitions [nfa current-states]
  (loop [current-states current-states]
        (let [eps-transitions (next-states-nfa nfa current-states :eps)]
          (if (empty? (clojure.set/difference eps-transitions current-states))
            current-states
            (recur (clojure.set/union current-states eps-transitions))))))

(defn- accept-nfa-with-current-states [nfa string current-states]
  (loop [nfa nfa
         string string
         current-states current-states]
    (if (empty? string)
      (is-accepting-nfa? nfa (do-eps-transitions nfa current-states))
      (recur nfa (apply str (rest string)) (next-states-nfa nfa (do-eps-transitions nfa current-states) (first string))))))

(defn accept-nfa? [nfa string]
  (accept-nfa-with-current-states nfa (apply str string) #{(:starting-state nfa)}))
