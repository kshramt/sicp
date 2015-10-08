(ns sicp.core
  (:require
   [clojure.test :refer [is are deftest]]
   [sicp.util
    :refer [
            p_
            ]]
   [sicp.scheme]
   )
  )

(defn -main [& args]
  (sicp.scheme/main *command-line-args*))
