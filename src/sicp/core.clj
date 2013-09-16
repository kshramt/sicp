(ns sicp.core
  (:require [clojure.test :refer [is are run-tests]]
            [clojure.pprint]
            [clojure.math.numeric-tower]
            [clojure.repl]
            [clojure.core.typed :refer [ann-form ann AnyInteger letfn> loop>] :as typed]))

(ann clojure.pprint/pprint [Any -> nil])
(ann clojure.core/mod [Number Number -> Number])
(typed/non-nil-return clojure.lang.Numbers/addP :all)
(typed/non-nil-return clojure.lang.Numbers/minusP :all)
(typed/non-nil-return clojure.lang.Numbers/multiplyP :all)
(typed/non-nil-return clojure.lang.Numbers/remainder :all)

(ann p_ (All [a] [a -> a]))
(defn p_
  "Pretty print and return a value"
  [x]
  (clojure.pprint/pprint x)
  x)

(ann twice (Fn [AnyInteger -> AnyInteger]
               [Number -> Number]))
(defn twice [x]
  (+ x x))

(ann square (Fn [AnyInteger -> AnyInteger]
                [Number -> Number]))
(defn square [x]
  (* x x))

(ann cube (Fn [AnyInteger -> AnyInteger]
              [Number -> Number]))
(defn cube [x]
  (* x (square x)))

(ann half [Number -> Number])
(defn half [x]
  (/ x 2))

(ann abs (Fn [AnyInteger -> AnyInteger]
             [Number -> Number]))
(defn abs [x]
  (if (neg? x)
    (* -1 x)
    x))

(ann third-root [Number -> Number])
(defn third-root
  [x]
  (letfn>
      [improved-guess :- [Number -> Number]
       (improved-guess [guess]
                       (/ (+ (/ x (square guess)) (twice guess)) 3))
       enough-precision? :- [Number -> Boolean]
       (enough-precision? [guess]
                          (< (abs (- x (cube guess))) 0.0001))]
    (loop> [x :- Number x
            guess :- Number 1.0]
      (if (enough-precision? guess)
        guess
        (recur x (improved-guess guess))))))

(ann my-expt [Number AnyInteger -> Number])
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

  (loop> [b :- Number b
          n :- AnyInteger n
          a :- Number 1]
    (cond
     (zero? n) a
     (odd? n) (* a b (my-expt b (dec n)))
     :else (recur (square b) (half n) a))))

(ann my-* (Fn [AnyInteger AnyInteger -> AnyInteger]
              [Number AnyInteger -> Number]))
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


(ann my-expt-with-my-* [AnyInteger AnyInteger -> AnyInteger])
(defn my-expt-with-my-*
  "Q 1.18"
  ([b n]
     {:pre [(>= n 0)]}

     (letfn> [square-with-my-* :- [AnyInteger -> AnyInteger]
              (square-with-my-* [n]
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

(ann fib [AnyInteger -> AnyInteger])
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

  (letfn> [fib-iter :- [AnyInteger
                        AnyInteger
                        AnyInteger
                        AnyInteger
                        AnyInteger
                        ->
                        AnyInteger]
          (fib-iter [a b p q n]
            {:pre [(>= n 0)]}

           (loop> [a :- AnyInteger a
                   b :- AnyInteger b
                   p :- AnyInteger p
                   q :- AnyInteger q
                   n :- AnyInteger n]
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
                           (long (/ n 2))))))]
    (fib-iter 1 0 0 1 n)))

(ann rem' [Number Number -> Number])
(defn rem' [m n]
  (rem m n))

(ann gcd [AnyInteger AnyInteger -> AnyInteger])
(defn gcd
  {:test #(do (are [m n result] (= (gcd m n) result)
                   45 15 15
                   3 8 1
                   46 22 2))}

  [m n]
  (let [m_ (if (<= m n) m n)
        n_ (if (<= m n) n m)]
    (if (= m_ 0)
      n_
      (recur (rem' n_ m_) m_))))

(ann smallest-divisor [AnyInteger -> AnyInteger])
(defn smallest-divisor
  {:test #(do (are [n result] (= (smallest-divisor n) result)
                    8 2
                    23 23))}

  [n]
  {:pre [(>= n 1)]}

  (letfn> [divides? :- [AnyInteger AnyInteger -> Boolean]
           (divides? [divisor n]
             {:pre [(>= n divisor)]}
             (zero? (rem' n divisor)))

           find-divisor :- [AnyInteger AnyInteger -> AnyInteger]
           (find-divisor [n test-divisor]
             (cond
              (> (square test-divisor) n) n
              (divides? test-divisor n) test-divisor
              :else (find-divisor n (inc test-divisor))))]

    (find-divisor n 2)))

(ann prime? [AnyInteger -> Boolean])
(defn prime?
  {:test #(do (are [n] (prime? n)
                   2
                   3
                   5
                   7
                   11
                   13))}

  [n]
  {:pre [(>= n 2)]}

  (= n (smallest-divisor n)))

(ann sum-integers [AnyInteger AnyInteger -> AnyInteger])
(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a (sum-integers (inc a) b))))

(ann sum-cubes [AnyInteger AnyInteger -> AnyInteger])
(defn sum-cubes [a b]
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (inc a) b))))

(ann pi-sum [AnyInteger AnyInteger -> Number])
(defn pi-sum [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(ann sum' (Fn [[AnyInteger -> Number]
               AnyInteger
               [AnyInteger -> AnyInteger]
               AnyInteger
               ->
               Number]
              [[AnyInteger -> Number]
               AnyInteger
               [AnyInteger -> AnyInteger]
               AnyInteger
               Number
               ->
               Number]))
(defn sum'
  {:test #(do (is (= 55 (sum' identity 1 inc 10))))}
  ([term a next b] (sum' term a next b 0))
  ([term a next b ret]
     (if (> a b)
      ret
      (recur term (next a) next b (+ ret (term a))))))

(ann sum-cubes' [AnyInteger AnyInteger -> Number])
(defn sum-cubes' [a b]
  (sum' cube a inc b))

(ann num-identity (All [[a :< Number]]
                     (Fn [a -> a]
                         [AnyInteger -> AnyInteger]
                         [Number -> Number])))
(defn num-identity [x]
  x)

(ann sum-integers' [AnyInteger AnyInteger -> Number])
(defn sum-integers' [a b]
  (sum' num-identity a inc b))

(ann pi-sum' [AnyInteger AnyInteger -> Number])
(defn pi-sum' [a b]
  (letfn> [pi-term :- [AnyInteger -> Number]
           (pi-term [x]
             (/ 1.0 (* x (+ x 2))))
           pi-next :- [AnyInteger -> AnyInteger]
           (pi-next [x]
             (+ x 4))]
    (sum' pi-term a pi-next b)))

(ann product' (Fn [[Number -> Number]
                   Number
                   [Number -> Number]
                   Number
                   ->
                   Number]
                  [[Number -> Number]
                   Number
                   [Number -> Number]
                   Number
                   Number
                   ->
                   Number]))
(defn product'
  {:test #(do (is (= 3628800 (product' identity 1 inc 10))))}
  ([term a next b] (product' term a next b 1))
  ([term a next b ret]
    (if (> a b)
      ret
      (recur term (next a) next b (* ret (term a))))))


(ann accumulate' (All [a] [[a * -> a]
                           a
                           [AnyInteger -> a]
                           AnyInteger
                           [AnyInteger -> AnyInteger]
                           AnyInteger
                           ->
                           a]))
(defn accumulate'
  {:test #(do (is (= 3628800 (accumulate' * 1 identity 1 inc 10)))
              (is (= 55 (accumulate' + 0 identity 1 inc 10))))}
  [combiner null-value term a next b]
     (if (> a b)
       null-value
       (recur combiner (combiner null-value (term a)) term (next a) next b)))

(ann filtered-accumulate' (All [a] [[AnyInteger -> Boolean]
                                    [a * -> a]
                                    a
                                    [AnyInteger -> a]
                                    AnyInteger
                                    [AnyInteger -> AnyInteger]
                                    AnyInteger
                                    ->
                                    a]))
(defn filtered-accumulate'
  {:test #(do (is (= 3628800 (filtered-accumulate' pos? * 1 identity -1 inc 10)))
              (is (= 55 (filtered-accumulate' pos? + 0 identity -1 inc 10))))}
  [pred combiner null-value term a next b]
  (if (> a b)
    null-value
    (if (pred a)
      (recur pred combiner (combiner null-value (term a)) term (next a) next b)
      (recur pred combiner null-value term (next a) next b))))

(ann square-sum-of-primes [AnyInteger AnyInteger -> AnyInteger])
(defn square-sum-of-primes
  {:test #(do (is (= (+ 4 9 25) (square-sum-of-primes 2 5))))}
  [a b]
  (filtered-accumulate' prime? + 0 square a inc b))

(ann product-of-coprimes [AnyInteger AnyInteger -> AnyInteger])
(defn product-of-coprimes
  {:test #(do (is (= (* 1 3 5 7) (product-of-coprimes 1 8))))}
  [a b]
  (letfn> [pos-and-coprime? :- [AnyInteger -> Boolean]
           (pos-and-coprime?
             [i]
             (and (pos? i) (= 1 (gcd i b))))]
    (filtered-accumulate' pos-and-coprime? * 1 num-identity a inc b)))

(ann close-enough? [Number Number -> Boolean])
(defn close-enough? [x y]
  (< (abs (- x y)) 0.001))

(ann average [Number Number -> Number])
(defn average [x y]
  (/ (+ x y) 2))

(ann search [[Number -> Number]
             Number
             Number
             ->
             Number])
(defn search [f neg-point pos-point]
  (let [mid-point (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      mid-point
      (let [test-value (f mid-point)]
        (cond
         (pos? test-value) (search f neg-point mid-point)
         (neg? test-value) (search f mid-point pos-point)
         :else mid-point)))))

(ann half-interval-method [[Number -> Number]
                           Number
                           Number
                           ->
                           Number])
(defn half-interval-method
  {:test #(do (is (close-enough?
                   (half-interval-method (fn [x] (- (square x) 2)) 1.0 5.0)
                   (Math/sqrt 2))))}
  [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond
     (zero? a-value) a
     (zero? b-value) b
     (and (neg? a-value) (pos? b-value)) (search f a b)
     (and (neg? b-value) (pos? a-value)) (search f b a)
     :else (throw (Exception. (str "Values are not of opposite sign: " a " " b))))))

(ann tolerance double)
(def tolerance 0.00001)

(ann fixed-point [[Number -> Number] Number -> Number])
(defn fixed-point [f first-guess]
  (letfn> [close-enough? :- [Number Number -> Boolean]
           (close-enough? [v1 v2]
             (< (abs (- v1 v2)) tolerance))
           try_ :- [Number -> Number]
           (try_ [guess]
             (let [next (f guess)]
               (if (close-enough? guess next)
                 next
                 (try_ next))))]
    (try_ first-guess)))

(ann fixed-point' [[Number -> Number] Number -> Number])
(defn fixed-point' [f first-guess]
  (letfn> [close-enough? :- [Number Number -> Boolean]
           (close-enough? [v1 v2]
             (< (abs (- v1 v2)) tolerance))
           try_ :- [Number -> Number]
           (try_ [guess]
             (let [next (f guess)]
               (println next)
               (if (close-enough? guess next)
                 next
                 (recur next))))]
    (try_ first-guess)))

(ann sqrt' [Number -> Number])
(defn sqrt' [x]
  {:pre [(>= x 0)]}
  (fixed-point
   (ann-form
    #(average % (/ x %))
    [Number -> Number])
   1.0))

(ann sqrt'' [Number -> Number])
(defn sqrt'' [x]
  {:pre [(>= x 0)]}
  (fixed-point'
   (ann-form
    #(average % (/ x %))
    [Number -> Number])
   1.0))

(ann golden-ratio Number)
(def golden-ratio
  (fixed-point
   (ann-form
    #(average % (+ 1 (/ 1 %)))
    [Number -> Number])
   1.0))


(ann cont-frac [[AnyInteger -> Number]
                [AnyInteger -> Number]
                AnyInteger
                ->
                Number])
(defn cont-frac
  "Q. 1.37-a"
  {:test #(do (is (= (/ 1 2)
                     (cont-frac (fn [x] x) (fn [x] x) 2))))}
  [n d k]
  {:pre [(>= k 1)]}
  (letfn> [recur_ :- [[AnyInteger -> Number]
                      [AnyInteger -> Number]
                      AnyInteger
                      AnyInteger
                      ->
                      Number]
           (recur_ [n d i k]
                   (if (< i k)
                     (/ (n i)
                        (+ (d i)
                           (recur_ n d (inc i) k)))
                     (/ (n i)
                        (d i))))]
          (recur_ n d 1 k)))

(ann cont-frac' [[AnyInteger -> Number]
                 [AnyInteger -> Number]
                 AnyInteger
                 ->
                 Number])
(defn cont-frac'
  "Q. 1.37-b"
  {:test #(do (is (= (/ 1 2)
                     (cont-frac' (fn [x] x) (fn [x] x) 2))))}
  [n d k]
  {:pre [(>= k 1)]}
  (loop> [n :- [AnyInteger -> Number] n
          d :- [AnyInteger -> Number] d
          i :- AnyInteger k
          k :- AnyInteger k
          ret :- Number 0]
    (if (= i 0)
      ret
      (recur n d (dec i) k (/ (n i)
                              (+ (d i)
                                 ret))))))

(ann approx-e [AnyInteger -> Number])
(defn approx-e
  "Q. 1.38"
  [k]
  {:pre [(>= k 1)]}
  (+ (cont-frac' (fn [_] 1)
                 (ann-form (fn [x]
                             (if (= (mod x 3) 2)
                               (* 2
                                  (+ (/ (- x 2)
                                        3)
                                     1))
                               1))
                           [Number -> Number])
                 k)
     2))

(ann tan-cf [Number AnyInteger -> Number])
(defn tan-cf
  "Q. 1.39"
  [x k]
  (cont-frac' (ann-form #(if (= % 1) x (square x)) [AnyInteger -> Number])
              (ann-form #(- (* % 2) 1) [AnyInteger -> AnyInteger])
              k))

(ann average-damp [[Number -> Number] -> [Number -> Number]])
(defn average-damp [f]
  (fn [x] (average x (f x))))

;(run-tests)
