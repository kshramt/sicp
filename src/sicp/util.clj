(ns sicp.util
  (:require
   [clojure.core.typed
    :refer [
            IFn
            Int
            Num
            ann
            ]
    :as typed]
   [clojure.pprint]
   [clojure.math.numeric-tower
    ]
   )
  )


(typed/override-method clojure.lang.Numbers/remainder (IFn [Int Int -> Int]
                                                           [Num Num -> Num]))
(typed/override-method clojure.lang.Numbers/isNeg [Num -> Boolean])
(ann ^:no-check clojure.math.numeric-tower/sqrt [Num -> Num])


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
