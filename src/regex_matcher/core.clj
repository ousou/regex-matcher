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

(def or-starting-state "or-starting-state")

(defn- create-union-starting-state [nfa-1 nfa-2]
  or-starting-state)

(defn- add-prefix-to-states [prefix states]
  (into #{} (map (partial str prefix) states)))

(defn- create-union-states [nfa-1 nfa-2]
  (clojure.set/union (into #{} or-starting-state) (add-prefix-to-states "1-" (:states nfa-1)) (add-prefix-to-states "2-" (:states nfa-2))))

(defn- create-union-accepting-states [nfa-1 nfa-2]
  (clojure.set/union (add-prefix-to-states "1-" (:accepting-states nfa-1)) (add-prefix-to-states "2-" (:accepting-states nfa-2))))

(defn- create-transitions-for-new-starting-state
  "Creates epsilon transitions for the new starting state to the old, renamed, starting states of the original nfas"
  [nfa-1 nfa-2]
  {{:state or-starting-state :char :eps} #{(str "1-" (:starting-state nfa-1)) (str "2-" (:starting-state nfa-2))}})

(defn- add-prefix-to-state
  "Adds the provided prefix to the state in the key of the given map entry from the transition function"
  [prefix map-entry]
  (let [state-and-char (first map-entry)
        next-states (second map-entry)
        state (:state state-and-char)
        new-state (str prefix state)
        char (:char state-and-char)
        new-next-states (add-prefix-to-states prefix next-states)]
      {{:state new-state :char char} new-next-states}))

(defn- add-prefix-to-states-in-transition-func [prefix transition-func]
  (into {} (map (partial add-prefix-to-state prefix) transition-func)))

(defn- create-union-transition-func [nfa-1 nfa-2]
  (merge (create-transitions-for-new-starting-state nfa-1 nfa-2)
         (add-prefix-to-states-in-transition-func "1-" (:transition-function nfa-1))
         (add-prefix-to-states-in-transition-func "2-" (:transition-function nfa-2))))

(defn create-union-nfa
  "Creates a new nfa that accepts the union of the strings of the two given nfas"
  [nfa-1 nfa-2]
  (->nfa (create-union-states nfa-1 nfa-2) (create-union-transition-func nfa-1 nfa-2) (create-union-accepting-states nfa-1 nfa-2) (create-union-starting-state nfa-1 nfa-2)))
