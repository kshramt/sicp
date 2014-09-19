(ns sicp.ch-3
  (:require [clojure.test :refer [is are deftest]]
            [clojure.pprint]
            [clojure.core.typed :refer [ann
                                        fn> ; `typed/fn` is not usable in REPL
                                        Int Num
                                        Keyword
                                        ] :as typed]
            ;:verbose
            )
  (:gen-class))


(ann make-account [Num -> [Keyword -> [Num -> Num]]])
(defn make-account [balance]
  (let [local-balance (atom balance)]
    (typed/fn [m :- Keyword]
      (case m
        :withdraw (typed/fn [amount :- Num]
                    (let [b @local-balance]
                      (if (>= b amount)
                        (reset! local-balance (- b amount))
                        (throw (Exception. "Insufficient funds")))))
        :deposit (typed/fn [amount :- Num]
                   (reset! local-balance (+ @local-balance amount)))
        :else (throw (Exception. (str "Unknown request: " m)))))))


(ann make-accumulator [Num -> [Num -> Num]])
(defn make-accumulator
  "Q. 3.1"
  [x]
  (let [sum_ (atom x)]
    (typed/fn [x :- Num]
      (reset! sum_ (+ @sum_ x)))))


(typed/check-ns)
