(ns sicp.core
  (:require [clojure.test :as c.t])
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
  {:test #(do (c.t/is (= (fast-expt2-iter 3 4 1) 81)))}
  [b n a]
  {:pre [(>= n 0)]}
  (cond
   (zero? n) a
   (even? n) (fast-expt2-iter (square b) (half n) a)
   :else (fast-expt2-iter b (dec n) (* a b))))

(defn fast-expt2
  "Yet another fast exponential"
  {:test #(do (c.t/is (= (fast-expt2 3 4) 81)))}
  [b n]
  {:pre [(>= n 0)]}
  (fast-expt2-iter b n 1))
