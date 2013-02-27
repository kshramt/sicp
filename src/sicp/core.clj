(ns sicp.core
  (:require [clojure.test :refer [is are run-tests]])
  (:require [clojure.pprint]))

(defn p_
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

(defn my-expt
  "Q 1.16"
  {:test #(do (are [b n result] (= (my-expt b n) result)
                   0 0 1
                   0 1 0
                   1 0 1
                   3 4 81)
              (is (thrown? java.lang.AssertionError (my-expt 3 -1))))}

  [b n]
  {:pre [(>= n 0)]}

  (loop [b b n n a 1]
    (cond
     (zero? n) a
     (odd? n) (* a b (my-expt b (dec n)))
     :else (recur (square b) (half n) a))))

(defn my-*
  "Q. 1.17"
  {:test #(do (are [a b result] (= (my-* a b) result)
                   0 0 0
                   1 0 0
                   0 1 0
                   1 1 1
                   1 2 2
                   2 1 2
                   3 4 12
                   -3 4 -12)
              (is (thrown? java.lang.AssertionError (my-* 1 -1))))}

  [a b]
  {:pre [(>= b 0)]}

  (cond
   (zero? b) 0
   (odd? b) (+ a (my-* a (dec b)))
   :else (recur (twice a) (half b))))


(defn my-expt-with-my-*
  "Q 1.18"
  ([b n]
     {:pre [(>= n 0)]}

     (letfn
         [(square-with-my-* [n]
            (my-* n n))]

       (cond
        (zero? n) 1
        (odd? n) (my-* b (my-expt-with-my-* b (dec n)))
        :else (recur (square-with-my-* b) (half n)))))

  {:test #(do (are [b n result] (= (my-expt-with-my-* b n) result)
                   0 0 1
                   0 1 0
                   1 0 1
                   3 4 81)
              (is (thrown? java.lang.AssertionError (my-expt-with-my-* 3 -1))))})

(defn fib
  "Q 1.19"
  {:test #(do (are [n result] (= (fib n) result)
                   1 1
                   2 1
                   3 2
                   4 3
                   5 5
                   6 8
                   7 13
                   8 21
                   9 34
                   10 55)
              (is (thrown? java.lang.AssertionError (fib 0))))}

  [n]
  {:pre [(>= n 1)]}

  (letfn [(fib-iter
            [a b p q n]
            {:pre [(>= n 0)]}

            (loop [a a b b p p q q n n]
              (cond
               (= n 0) b
               (odd? n) (fib-iter (+ (* b q) (* a (+ p q)))
                                  (+ (* b p) (* a q))
                                  p
                                  q
                                  (dec n))
               :else (recur a
                            b
                            (+ (* p p) (* q q))
                            (+ (* 2 p q) (* q q))
                            (/ n 2)))))]
    (fib-iter 1 0 0 1 n)))

(run-tests)
