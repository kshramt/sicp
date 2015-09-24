(ns sicp.scheme-util
  (:require
   [clojure.test :refer [is are deftest]]
   [sicp.util
    :refer [
            p_
            pef
            ]]
   [sicp.pair
    :refer [
            List
            any?
            car
            caadr
            caar
            cadr
            caddr
            cadddr
            cdadr
            cdar
            cdr
            cddr
            cddr
            cdddr
            my-cons
            my-list
            my-map
            pair?
            set-car!
            set-cdr!
            ]
    ]
   )
  (:import [sicp.pair Pair]
           )
  )

(defn error
  ([] (error ""))
  ([msg] (error msg {}))
  ([msg map] (throw (ex-info msg map))))

(def null nil)
(def _nil (symbol "nil"))
(def _true (symbol "true"))
(def _false (symbol "false"))
