(ns regex-matcher.core)

(defrecord dfa [states transition-function accepting-states starting-state])

(defn- next-state [dfa current-state char]
  (get (:transition-function dfa) {:state current-state :char char}))

(defn- is-accepting? [dfa current-state]
    (contains? (:accepting-states dfa) current-state))

(defn- accept-with-current-state [dfa string current-state]
  (if (empty? string)
    (is-accepting? dfa current-state)
    (accept-with-current-state dfa (apply str (rest string)) (next-state dfa current-state (first string)))))

(defn accept? [dfa string]
  (accept-with-current-state dfa (apply str string) (:starting-state dfa)))
