(ns sicp.core
  (:require [clojure.test :refer [is are run-tests]])
  (:require [clojure.pprint]))

(defn p
  "Pretty print and return a value"
  [x]
  (clojure.pprint/pprint x)
  x)

(defn twice [x]
  (+ x x))

(defn square [x]
  (* x x))

(defn cube [x]
  (* x (square x)))

(defn half [x]
  (/ x 2))

(defn third-root
  [x]
  (letfn
      [(improved-guess
         [guess]
         (/ (+ (/ x (square guess)) (twice guess)) 3))
       (enough-precision?
         [guess]
         (< (Math/abs (- x (cube guess))) 0.0001))]
    (loop [x x guess 1.0]
      (if (enough-precision? guess)
        guess
        (recur x (improved-guess guess))))))

(defn fast-expt2-iter
  "Substance of fast-expt2."
  {:test #(do (is (= (fast-expt2-iter 3 4 1) 81)))}
  [b n a]
  {:pre [(>= n 0)]}
  (cond
   (zero? n) a
   (even? n) (fast-expt2-iter (square b) (half n) a)
   :else (fast-expt2-iter b (dec n) (* a b))))

(defn fast-expt2
  "Yet another fast exponential"
  {:test #(do (are [b n result] (= (fast-expt2 b n) result)
                   0 0 1
                   0 1 0
                   1 0 1
                   3 4 81)
              (is (thrown? java.lang.AssertionError (fast-expt2 3 -1))))}
  [b n]
  {:pre [(>= n 0)]}
  (fast-expt2-iter b n 1))

(run-tests)
