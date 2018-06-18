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

(defn accept-dfa?
  "Returns true if the given dfa accepts the given string."
  [dfa string]
  (accept-with-current-state dfa (apply str string) (:starting-state dfa)))

(defn- next-states-nfa [nfa current-states char]
  (loop [current-states current-states
         next-states #{}]
     (if (empty? current-states)
       next-states
       (recur (into #{} (rest current-states)) (clojure.set/union next-states (next-state nfa (first current-states) char))))))

(defn- is-accepting-nfa? [nfa current-states]
    (not (empty? (clojure.set/intersection (:accepting-states nfa) current-states))))

(defn- do-eps-transitions [nfa current-states]
  (loop [current-states current-states]
        (let [eps-transitions (next-states-nfa nfa current-states :eps)]
          (if (empty? (clojure.set/difference eps-transitions current-states))
            current-states
            (recur (clojure.set/union current-states eps-transitions))))))

(defn- accept-nfa-with-current-states [nfa string current-states]
  (loop [string string
         current-states current-states]
    (cond (empty? current-states) false
          (empty? string) (is-accepting-nfa? nfa (do-eps-transitions nfa current-states))
          :else (recur (apply str (rest string)) (next-states-nfa nfa (do-eps-transitions nfa current-states) (first string))))))

(defn accept-nfa?
  "Returns true if the given nfa accepts the given string."
  [nfa string]
  (accept-nfa-with-current-states nfa (apply str string) #{(:starting-state nfa)}))
