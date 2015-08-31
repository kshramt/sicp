(ns sicp.util
  (:require
   [clojure.core.typed
    :refer [
            Num
            ann
            ]
    :as typed]
   [clojure.pprint]
   [clojure.math.numeric-tower
    ]
   )
  )


(ann ^:no-check sqrt [Num -> Num])
(def sqrt clojure.math.numeric-tower/sqrt)


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
