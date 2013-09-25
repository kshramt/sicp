(ns sicp.core
  (:require [clojure.test :refer [is are]]
            [clojure.pprint]
            [clojure.math.numeric-tower]
            [clojure.repl]
            [clojure.core.typed :refer [ann-form ann Int Num letfn> loop> Vec] :as typed])
  (:gen-class))

(ann clojure.pprint/pprint [Any -> nil])
(typed/override-method clojure.lang.Numbers/remainder (Fn [Int Int -> Int]
                                                          [Num Num -> Num]))
(ann clojure.core/rem (Fn [Int Int -> Int]
                          [Num Num -> Num]))
(ann clojure.core/mod (Fn [Int Int -> Int]
                          [Num Num -> Num]))
(ann clojure.test/run-tests [clojure.lang.Namespace *
                             ->
                             (HMap :mandatory {:type clojure.lang.Keyword
                                               :pass Int
                                               :test Int
                                               :error Int
                                               :fail Int})])
(ann clojure.math.numeric-tower/ceil [Num -> Num])
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

(ann twice (Fn [Int -> Int]
               [Num -> Num]))
(defn twice [x]
  (+ x x))

(ann square (Fn [Int -> Int]
                [Num -> Num]))
(defn square [x]
  (* x x))

(ann cube (Fn [Int -> Int]
              [Num -> Num]))
(defn cube [x]
  (* x (square x)))

(ann half [Num -> Num])
(defn half [x]
  (/ x 2))

(ann abs (Fn [Int -> Int]
             [Num -> Num]))
(defn abs [x]
  (if (neg? x)
    (* -1 x)
    x))

(ann third-root [Num -> Num])
(defn third-root
  [x]
  (letfn>
      [improved-guess :- [Num -> Num]
       (improved-guess [guess]
                       (/ (+ (/ x (square guess)) (twice guess)) 3))
       enough-precision? :- [Num -> Boolean]
       (enough-precision? [guess]
                          (< (abs (- x (cube guess))) 0.0001))]
    (loop> [x :- Num x
            guess :- Num 1.0]
      (if (enough-precision? guess)
        guess
        (recur x (improved-guess guess))))))

(ann my-expt [Num Int -> Num])
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

  (loop> [b :- Num b
          n :- Int n
          a :- Num 1]
    (cond
     (zero? n) a
     (odd? n) (* a b (my-expt b (dec n)))
     :else (recur (square b) (half n) a))))

(ann my-* (Fn [Int Int -> Int]
              [Num Int -> Num]))
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


(ann my-expt-with-my-* [Int Int -> Int])
(defn my-expt-with-my-*
  "Q 1.18"
  ([b n]
     {:pre [(>= n 0)]}

     (letfn> [square-with-my-* :- [Int -> Int]
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

(ann fib [Int -> Int])
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

  (letfn> [fib-iter :- [Int
                        Int
                        Int
                        Int
                        Int
                        ->
                        Int]
          (fib-iter [a b p q n]
            {:pre [(>= n 0)]}

           (loop> [a :- Int a
                   b :- Int b
                   p :- Int p
                   q :- Int q
                   n :- Int n]
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

(ann gcd [Int Int -> Int])
(defn gcd
  {:test #(do (are [m n result] (= (gcd m n) result)
                   45 15 15
                   3 8 1
                   46 22 2
                   -46 22 2
                   46 -22 2
                   -46 -22 2))}

  [m n]
  (let [abs-m (abs m)
        abs-n (abs n)
        small (if (< abs-m abs-n) abs-m abs-n)
        large (if (> abs-m abs-n) abs-m abs-n)]
    (if (= small 0)
      large
      (recur (rem large small) small))))

(ann smallest-divisor [Int -> Int])
(defn smallest-divisor
  {:test #(do (are [n result] (= (smallest-divisor n) result)
                    8 2
                    23 23))}

  [n]
  {:pre [(>= n 1)]}

  (letfn> [divides? :- [Int Int -> Boolean]
           (divides? [divisor n]
             {:pre [(>= n divisor)]}
             (zero? (rem n divisor)))

           find-divisor :- [Int Int -> Int]
           (find-divisor [n test-divisor]
             (cond
              (> (square test-divisor) n) n
              (divides? test-divisor n) test-divisor
              :else (find-divisor n (inc test-divisor))))]

    (find-divisor n 2)))

(ann prime? [Int -> Boolean])
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

(ann sum-integers [Int Int -> Int])
(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a (sum-integers (inc a) b))))

(ann sum-cubes [Int Int -> Int])
(defn sum-cubes [a b]
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (inc a) b))))

(ann pi-sum [Int Int -> Num])
(defn pi-sum [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(ann sum' (Fn [[Int -> Num]
               Int
               [Int -> Int]
               Int
               ->
               Num]
              [[Int -> Num]
               Int
               [Int -> Int]
               Int
               Num
               ->
               Num]))
(defn sum'
  {:test #(do (is (= 55 (sum' identity 1 inc 10))))}
  ([term a next b] (sum' term a next b 0))
  ([term a next b ret]
     (if (> a b)
      ret
      (recur term (next a) next b (+ ret (term a))))))

(ann sum-cubes' [Int Int -> Num])
(defn sum-cubes' [a b]
  (sum' cube a inc b))

(ann num-identity (All [[a :< Num]]
                     (Fn [a -> a]
                         [Int -> Int]
                         [Num -> Num])))
(defn num-identity [x]
  x)

(ann sum-integers' [Int Int -> Num])
(defn sum-integers' [a b]
  (sum' num-identity a inc b))

(ann pi-sum' [Int Int -> Num])
(defn pi-sum' [a b]
  (letfn> [pi-term :- [Int -> Num]
           (pi-term [x]
             (/ 1.0 (* x (+ x 2))))
           pi-next :- [Int -> Int]
           (pi-next [x]
             (+ x 4))]
    (sum' pi-term a pi-next b)))

(ann product' (Fn [[Num -> Num]
                   Num
                   [Num -> Num]
                   Num
                   ->
                   Num]
                  [[Num -> Num]
                   Num
                   [Num -> Num]
                   Num
                   Num
                   ->
                   Num]))
(defn product'
  {:test #(do (is (= 3628800 (product' identity 1 inc 10))))}
  ([term a next b] (product' term a next b 1))
  ([term a next b ret]
    (if (> a b)
      ret
      (recur term (next a) next b (* ret (term a))))))


(ann accumulate' (All [a] [[a * -> a]
                           a
                           [Int -> a]
                           Int
                           [Int -> Int]
                           Int
                           ->
                           a]))
(defn accumulate'
  {:test #(do (is (= 3628800 (accumulate' * 1 identity 1 inc 10)))
              (is (= 55 (accumulate' + 0 identity 1 inc 10))))}
  [combiner null-value term a next b]
     (if (> a b)
       null-value
       (recur combiner (combiner null-value (term a)) term (next a) next b)))

(ann filtered-accumulate' (All [a] [[Int -> Boolean]
                                    [a * -> a]
                                    a
                                    [Int -> a]
                                    Int
                                    [Int -> Int]
                                    Int
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

(ann square-sum-of-primes [Int Int -> Int])
(defn square-sum-of-primes
  {:test #(do (is (= (+ 4 9 25) (square-sum-of-primes 2 5))))}
  [a b]
  (filtered-accumulate' prime? + 0 square a inc b))

(ann product-of-coprimes [Int Int -> Int])
(defn product-of-coprimes
  {:test #(do (is (= (* 1 3 5 7) (product-of-coprimes 1 8))))}
  [a b]
  (letfn> [pos-and-coprime? :- [Int -> Boolean]
           (pos-and-coprime?
             [i]
             (and (pos? i) (= 1 (gcd i b))))]
    (filtered-accumulate' pos-and-coprime? * 1 num-identity a inc b)))

(ann close-enough? (Fn [Num Num -> Boolean]
                       [Num Num Num -> Boolean]))
(defn close-enough?
  ([x y] (close-enough? x y 0.001))
  ([x y delta]
     {:pre [(>= delta 0)]}
     (<= (abs (- x y)) delta)))

(ann average [Num Num -> Num])
(defn average [x y]
  (/ (+ x y) 2))

(ann search [[Num -> Num]
             Num
             Num
             ->
             Num])
(defn search [f neg-point pos-point]
  (let [mid-point (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      mid-point
      (let [test-value (f mid-point)]
        (cond
         (pos? test-value) (search f neg-point mid-point)
         (neg? test-value) (search f mid-point pos-point)
         :else mid-point)))))

(ann half-interval-method [[Num -> Num]
                           Num
                           Num
                           ->
                           Num])
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

(ann fixed-point [[Num -> Num] Num -> Num])
(defn fixed-point [f first-guess]
  (letfn> [try_ :- [Num -> Num]
           (try_ [guess]
             (let [next (f guess)]
               (if (close-enough? guess next tolerance)
                 next
                 (try_ next))))]
    (try_ first-guess)))

(ann fixed-point' [[Num -> Num] Num -> Num])
(defn fixed-point' [f first-guess]
  (letfn> [try_ :- [Num -> Num]
           (try_ [guess]
             (let [next (f guess)]
               (println next)
               (if (close-enough? guess next tolerance)
                 next
                 (recur next))))]
    (try_ first-guess)))

(ann sqrt' [Num -> Num])
(defn sqrt' [x]
  {:pre [(>= x 0)]}
  (fixed-point
   (ann-form
    #(average % (/ x %))
    [Num -> Num])
   1.0))

(ann sqrt'' [Num -> Num])
(defn sqrt'' [x]
  {:pre [(>= x 0)]}
  (fixed-point'
   (ann-form
    #(average % (/ x %))
    [Num -> Num])
   1.0))

(ann golden-ratio Num)
(def golden-ratio
  (fixed-point
   (ann-form
    #(average % (+ 1 (/ 1 %)))
    [Num -> Num])
   1.0))


(ann cont-frac [[Int -> Num]
                [Int -> Num]
                Int
                ->
                Num])
(defn cont-frac
  "Q. 1.37-a"
  {:test #(do (is (= (/ 1 2)
                     (cont-frac (fn [x] x) (fn [x] x) 2))))}
  [n d k]
  {:pre [(>= k 1)]}
  (letfn> [recur_ :- [[Int -> Num]
                      [Int -> Num]
                      Int
                      Int
                      ->
                      Num]
           (recur_ [n d i k]
                   (if (< i k)
                     (/ (n i)
                        (+ (d i)
                           (recur_ n d (inc i) k)))
                     (/ (n i)
                        (d i))))]
          (recur_ n d 1 k)))

(ann cont-frac' [[Int -> Num]
                 [Int -> Num]
                 Int
                 ->
                 Num])
(defn cont-frac'
  "Q. 1.37-b"
  {:test #(do (is (= (/ 1 2)
                     (cont-frac' (fn [x] x) (fn [x] x) 2))))}
  [n d k]
  {:pre [(>= k 1)]}
  (loop> [n :- [Int -> Num] n
          d :- [Int -> Num] d
          i :- Int k
          k :- Int k
          ret :- Num 0]
    (if (= i 0)
      ret
      (recur n d (dec i) k (/ (n i)
                              (+ (d i)
                                 ret))))))

(ann approx-e [Int -> Num])
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
                           [Num -> Num])
                 k)
     2))

(ann tan-cf [Num Int -> Num])
(defn tan-cf
  "Q. 1.39"
  [x k]
  (cont-frac' (ann-form #(if (= % 1) x (* -1 (square x))) [Int -> Num])
              (ann-form #(- (* % 2) 1) [Int -> Int])
              k))

(ann average-damp [[Num -> Num] -> [Num -> Num]])
(defn average-damp [f]
  (fn [x] (average x (f x))))

(ann dx double)
(def dx 0.00001)

(ann deriv [[Num -> Num] -> [Num -> Num]])
(defn deriv [f]
  (fn [x] (/ (- (f (+ x dx)) (f (- x dx)))
             (twice dx))))

(ann newton-transform [[Num -> Num] -> [Num -> Num]])
(defn newton-transform [f]
  (fn [x] (- x
             (/ (f x)
                ((deriv f) x)))))

(ann newton-method [[Num -> Num] Num -> Num])
(defn newton-method [f guess]
  (fixed-point (newton-transform f) guess))

(ann sqrt''' [Num -> Num])
(defn sqrt''' [x]
  {:pre [(>= x 0)]}
  (newton-method (ann-form #(- (square %) x) [Num -> Num])
                 1.0))

(ann fixed-point-of-transform [[Num -> Num]
                               [[Num -> Num] -> [Num -> Num]]
                               Num
                               ->
                               Num])
(defn fixed-point-of-transform [f transform guess]
  (fixed-point (transform f) guess))

(ann sqrt'''' [Num -> Num])
(defn sqrt'''' [x]
  {:pre [(>= x 0)]}
  (fixed-point-of-transform (ann-form #(/ x %) [Num -> Num])
                            average-damp
                            1.0))

(ann sqrt''''' [Num -> Num])
(defn sqrt''''' [x]
  {:pre [(>= x 0)]}
  (fixed-point-of-transform (ann-form #(- (square %) x) [Num -> Num])
                            newton-transform
                            1.0))

(ann cubic [Num Num Num -> [Num -> Num]])
(defn cubic
  "Q. 1.40"
  {:test #(do (is (< (abs (- (newton-method (cubic 2 3 -22) 1) 2)) tolerance)))}
  [a b c]
       (fn [x] (+ c (* x (+ b (* x (+ a (* x (+ 1)))))))))

(ann double_ [[Any -> Any] -> [Any -> Any]])
(defn double_
  "Q. 1.41"
  [f]
  (fn [x] (f (f x))))

(ann compose (All [a b c] [[b -> c] [a -> b] -> [a -> c]]))
(defn compose
  "Q. 1.42"
  [f g] (fn [x] (f (g x))))

(ann repeated (All [a] (Fn [[a -> a] Int -> [a -> a]]
                           [[a -> a] Int [a -> a] -> [a -> a]])))
(defn repeated
  "Q. 1.43"
  {:test #(do (is (= 5 ((repeated inc 5) 0))))}
  ([f n] (repeated f n identity))
  ([f n ret]
     {:pre [(>= n 0)]}
     (cond
      (zero? n) ret
      (zero? (mod n 2)) (recur (compose f f) (half n) ret)
      :else (recur f (dec n) (fn [x] (f (ret x)))))))

(ann smooth (Fn [[Num -> Num] -> [Num -> Num]]
                [[Num -> Num] Num -> [Num -> Num]]))
(defn smooth
  "Q. 1.44-1"
  ([f] (smooth f 0.00001))
  ([f dx] (fn [x] (/ (+ (f (- x dx))
                        (twice (f x))
                        (f (+ x dx)))
                     4))))

(ann smooth-n [[Num -> Num] Int -> [Num -> Num]])
(defn smooth-n
  "Q. 1.44-2"
  ([f n] ((repeated smooth n) f)))

(ann log2 [Num -> Num])
(defn log2 [x]
  (/ (Math/log (double x)) (Math/log 2.0)))

(ann nth-root [Num Int -> Num])
(defn nth-root
  "Q. 1.45"
  {:test #(do (is (< (- (nth-root 32 5) 2) tolerance)))}
  [x n]
  (letfn> [damp :- [Int
                    ->
                    [[Num -> Num] -> [Num -> Num]]]
           (damp [n]
                 (repeated average-damp (dec (bigint (clojure.math.numeric-tower/ceil (log2 n))))))
           basic :- [Num -> Num]
           (basic [guess]
                  (/ x
                     (Math/pow (double guess) (double (dec n)))))]
          (fixed-point ((damp n) basic) 1)))

(ann iterative-improve [[Num -> Boolean] [Num -> Num] -> [Num -> Num]])
(defn iterative-improve
  "Q. 1.46-1"
  [is-good? update]
  (fn [x]
    (loop> [guess :- Num 1]
      (if (is-good? guess)
        guess
        (recur (update guess))))))

(ann sqrt'''''' [Num -> Num])
(defn sqrt''''''
  "Q. 1.46-2"
  {:test #(do (is (close-enough? (sqrt'''''' 4) 2 tolerance)))}
  [x]
  ((iterative-improve (ann-form (fn [guess] (close-enough? guess (/ x guess) tolerance))
                                [Num -> Boolean])
                      (average-damp (ann-form (fn [guess] (/ x guess))
                                              [Num -> Num])))
   x))

(ann fixed-point'' [[Num -> Num] Num -> Num])
(defn fixed-point''
  "Q. 1.46-3"
  [f first-guess]
  (iterative-improve (ann-form (fn [guess] (close-enough? guess (f guess)))
                               [Num -> Boolean])
                     f)
  first-guess)

(typed/def-alias Rat (I (Vec Int) (CountRange 2 2)))

(ann make-rat [Int Int -> Rat])
(defn make-rat [n d]
  (let [g (gcd n d)]
       [(bigint (/ n g)) (bigint (/ d g))]))

(ann numer [Rat -> Int])
(defn numer [x]
  (first x))

(ann denom [Rat -> Int])
(defn denom [x]
  (second x))

(ann print-rat [Rat -> nil])
(defn print-rat [x]
  (println (numer x) "/" (denom x)))

(ann add-rat [Rat Rat -> Rat])
(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(ann neg-rat [Rat -> Rat])
(defn neg-rat [x]
  (make-rat (* -1 (numer x))
            (denom x)))

(ann sub-rat [Rat Rat -> Rat])
(defn sub-rat [x y]
  (add-rat x (neg-rat y)))

(ann mul-rat [Rat Rat -> Rat])
(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(ann inv-rat [Rat -> Rat])
(defn inv-rat [x]
  (make-rat (denom x)
            (numer x)))

(ann div-rat [Rat Rat -> Rat])
(defn div-rat [x y]
  (mul-rat x
           (inv-rat y)))
(ann equal-rat? [Rat Rat -> Boolean])
(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

;(print-rat (add-rat (make-rat 1 3) (make-rat 3 3)))

; (clojure.core.typed/check-ns 'sicp.core)(clojure.test/run-tests 'sicp.core)
(ann -main [String * -> nil])
(defn -main [& args]
  (clojure.test/run-tests)
  (println "ok"))
