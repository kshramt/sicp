(ns sicp.util
  (:require [clojure.core.typed
             :as typed]
            [clojure.pprint])
  )


(defmacro p_ [x]
  `(let [x# ~x]
     (println ~(str &form))
     (clojure.pprint/pprint x#)
     x#))


(defmacro pef
  "print-env-form"
  [form]
  `(let [RETURN# ~form]
     (typed/print-env ~(str form))
     RETURN#))
