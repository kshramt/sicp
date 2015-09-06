(ns sicp.stream
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [All
            Any
            ASeq
            CountRange
            ExactCount
            I
            IFn
            Int
            Kw
            NonEmptySeqable
            Num
            Option
            Rec
            Seqable
            TFn
            U
            Val
            ann
            defalias
            letfn>
            ]
    :as typed
    ]
   [clojure.core.typed.unsafe
    :refer
    [
     ignore-with-unchecked-cast
     ]]
   [sicp.pair
    :refer [
            List
            any?
            car
            cadr
            cddr
            cdr
            my-cons
            my-list
            pair?
            set-car!
            set-cdr!
            ]
    ]
   [clojure.math.numeric-tower
    :refer
    [
     abs
     ]]
   [sicp.util
    :refer [
            p_
            pef
            ]]
   )
  (:import [sicp.pair Pair]))


(defalias Delay (TFn [[a :variance :covariant]] [-> a]))
(defalias AbstractStream (TFn [[a :variance :covariant]] (Rec [this] (Pair a (Delay (Option this))))))
(defalias Finite [-> false])
(defalias Infinite [-> true])
(defalias FiniteStream (TFn [[a :variance :covariant]] (I Finite (AbstractStream a))))
(defalias InfiniteStream (TFn [[a :variance :covariant]] (I Infinite (AbstractStream a))))
(defalias Stream (TFn [[a :variance :covariant]] (U (FiniteStream a) (InfiniteStream a))))


(ann ^:no-check memo-proc (All [a] [(Delay a) -> (Delay a)]))
(defn memo-proc [proc]
  (let [already-run? (typed/atom :- Boolean false)
        result (typed/atom :- a nil)]
    (typed/fn []
      (if (not @already-run?)
        (do (reset! result (proc))
            (reset! already-run? true)
            @result)
        @result))))


(defmacro my-delay [exp]
  `(memo-proc (typed/fn [] ~exp)))


(ann ^:no-check my-cons-stream (All [a] (IFn [a (Delay nil) -> (FiniteStream a)]
                                             [a (Delay (FiniteStream a)) -> (FiniteStream a)]
                                             [a (Delay (Option (FiniteStream a))) -> (FiniteStream a)] ; todo: remove me
                                             [a (Delay (InfiniteStream a)) -> (InfiniteStream a)]
                                             [a (Delay (Stream a)) -> (Stream a)])))
(defn my-cons-stream [a b]
  (my-cons a b))


(defmacro cons-stream [a b]
  `(my-cons-stream ~a (my-delay ~b)))


(ann ^:no-check stream-null?
     (All [a]
          [(Option (Stream a))
           ->
           Boolean
           :filters
           {:then (is nil 0)
            :else (! nil 0)}]))
(def stream-null? nil?)


(ann the-empty-stream nil)
(def the-empty-stream nil)


(ann my-force (All [a] [(Delay a) -> a]))
(defn my-force [delayed-object]
  (delayed-object))


(ann ^:no-check stream-car (All [a] [(Stream a) -> a]))
(def stream-car car)


(ann ^:no-check stream-cdr (All [a] (IFn [(FiniteStream a) -> (Option (FiniteStream a))]
                                         [(InfiniteStream a) -> (InfiniteStream a)]
                                         [(Stream a) -> (Option (Stream a))] ; todo: remove me
                                         )))
(def stream-cdr (comp my-force cdr))


(ann stream-ref (All [a] [(Stream a) Int -> a]))
(defn stream-ref [s n]
  {:pre (>= n 0)}
  (if (= n 0)
    (stream-car s)
    (let [more (stream-cdr s)]
      (if (stream-null? more)
        (throw (Exception. (str "out of bound: " n)))
        (recur more (dec n))))))


(ann ^:no-check make-stream (All [a] [a a * -> (FiniteStream a)]))
(defn make-stream [& xs]
  (let [impl (typed/fn imp [s :- (Seqable a)]
               (if-let [s (seq s)]
                 (cons-stream (first s)
                              (imp (rest s)))
                 nil))]
    (impl xs)))


(ann to-list (All [a] (IFn [nil -> nil]
                           [(Stream a) -> (ASeq a)]
                           [(Option (Stream a)) -> (Option (ASeq a))] ; todo: remove me
                           )))
(defn to-list [stream]
  (if (stream-null? stream)
    nil
    (cons (stream-car stream)
          (to-list (stream-cdr stream)))))


(ann any (All [a] [[a -> Boolean] (Seqable a) -> Boolean]))
(defn any [pred xs]
  (if-let [s (seq xs)]
    (or (pred (first s))
        (recur pred (rest s)))
    false))


(ann ^:no-check stream-map
     (All [c a b ...]
          (IFn [[a -> c] nil -> nil]
               [[a -> c] (FiniteStream a) -> (FiniteStream c)]
               [[a -> c] (InfiniteStream a) -> (InfiniteStream c)]
               [[a -> c] (Stream a) -> (Stream c)]
               [[a b ... b -> c] (FiniteStream a) (FiniteStream b) ... b -> (FiniteStream c)]
               [[a b ... b -> c] (InfiniteStream a) (FiniteStream b) ... b -> (FiniteStream c)]
               [[a b ... b -> c] (FiniteStream a) (InfiniteStream b) ... b -> (FiniteStream c)]
               [[a b ... b -> c] (InfiniteStream a) (InfiniteStream b) ... b -> (InfiniteStream c)]
               [[a b ... b -> c] (Stream a) (Stream b) ... b -> (Stream c)]
               )))
(defn stream-map
  "Q. 3.50"
  {:test #(is (to-list
               (stream-map
                +
                (make-stream 1 2 3)
                (make-stream 9 8 7)))
              [10 10 10])}
  [proc & arguments]
  (if (any stream-null? arguments)
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car arguments))
      (apply stream-map
             (cons proc (map stream-cdr arguments))))))


(ann stream-for-each (All [a] [[a a * -> Any] (Option (Stream a)) -> (Val :done)]))
(defn stream-for-each [proc s]
  (if (stream-null? s)
    :done
    (do (proc (stream-car s))
        (recur proc (stream-cdr s)))))


(ann display-stream (All [a] [(Option (Stream a)) -> (Val :done)]))
(defn display-stream [stream]
  (stream-for-each println stream))


(ann stream-enumerate-interval [Int Int -> (Option (FiniteStream Int))])
(defn stream-enumerate-interval [lo hi]
  (if (> lo hi)
    the-empty-stream
    (cons-stream
     lo
     (stream-enumerate-interval (inc lo) hi))))


(ann stream-filter
     (All [a]
          (IFn [[a -> Boolean] nil -> nil]
               [[a -> Boolean] (FiniteStream a) -> (Option (FiniteStream a))]
               [[a -> Boolean] (Option (FiniteStream a)) -> (Option (FiniteStream a))] ; todo: remove me
               [[a -> Boolean] (InfiniteStream a) -> (InfiniteStream a)])))
(defn stream-filter [pred stream]
  (cond
    (stream-null? stream) the-empty-stream
    (pred (stream-car stream)) (cons-stream (stream-car stream)
                                            (stream-filter pred
                                                           (stream-cdr stream)))
    :else ;(recur pred (stream-cdr stream)) ; todo: wait for core.type's bug fix
    (stream-filter pred (stream-cdr stream))))


(typed/tc-ignore
(defn q-3-51
  "Q. 3.51"
  []
  (let [x (stream-map println (stream-enumerate-interval 0 10))]
    (stream-ref x 5)
    (println "")
    (stream-ref x 7)))
; Q. 3.52 skip
) ; typed/tc-ignore


(ann integers-starting-from [Int -> (InfiniteStream Int)])
(defn integers-starting-from [n]
  (cons-stream n (integers-starting-from (inc n))))


(ann integers (InfiniteStream Int))
(def integers (integers-starting-from 1))


(ann divisible? [Int Int -> Boolean])
(defn divisible? [x y]
  (zero? (rem x y)))


(ann sieve [(InfiniteStream Int) -> (InfiniteStream Int)])
(defn sieve [stream]
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (typed/fn [n :- Int]
             (not (divisible? n (stream-car stream))))
           (stream-cdr stream)))))


(ann primes (InfiniteStream Int))
(def primes (sieve (integers-starting-from 2)))


; Q. 3.53 2^n


(ann add-streams (IFn [(FiniteStream Int) (FiniteStream Int) -> (FiniteStream Int)]
                      [(FiniteStream Num) (FiniteStream Num) -> (FiniteStream Num)]
                      [(InfiniteStream Int) (InfiniteStream Int) -> (InfiniteStream Int)]
                      [(InfiniteStream Num) (InfiniteStream Num) -> (InfiniteStream Num)]))
(defn add-streams [s1 s2]
  (stream-map + s1 s2))


(ann mul-streams (IFn [(FiniteStream Int) (FiniteStream Int) -> (FiniteStream Int)]
                      [(FiniteStream Num) (FiniteStream Num) -> (FiniteStream Num)]
                      [(InfiniteStream Int) (InfiniteStream Int) -> (InfiniteStream Int)]
                      [(InfiniteStream Num) (InfiniteStream Num) -> (InfiniteStream Num)]))
(defn mul-streams [s1 s2]
  (stream-map * s1 s2))


(ann scale-stream (IFn [nil Num -> nil]

                       [(FiniteStream Int) Int -> (FiniteStream Int)]
                       [(FiniteStream Num) Num -> (FiniteStream Num)]

                       [(InfiniteStream Int) Int -> (InfiniteStream Int)]
                       [(InfiniteStream Num) Num -> (InfiniteStream Num)]

                       ; todo: remove me
                       [(Stream Int) Int -> (Stream Int)]
                       [(Stream Num) Num -> (Stream Num)]
                       ))
(defn scale-stream [stream factor]
  (stream-map (partial * factor) stream))


(ann stream-take
     (All [a]
          (IFn [nil Int -> nil]
               [(Stream a) Int -> (Option (FiniteStream a))]
               [nil Int Int -> nil]
               [(Stream a) Int Int -> (Option (FiniteStream a))]
               [(Option (Stream a)) Int Int -> (Option (FiniteStream a))] ; todo: remove me
               )))
(defn stream-take
  ([s n] (stream-take s n 1))
  ([s n i]
   (if (or (> i n) (stream-null? s))
     the-empty-stream
     (cons-stream (stream-car s)
                  (stream-take (stream-cdr s)
                               n
                               (inc i))))))

; Q. 3.54
(ann factorials (InfiniteStream Int))
(def factorials (cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))


(defmacro def-stream
  ([name head body]
   `(let [~name (cons-stream ~head nil)]
      (set-cdr! ~name (my-delay
                       ~body))
      ~name))
  ([name head body t]
   `(let [~name (ignore-with-unchecked-cast (cons-stream ~head nil) ~t)]
      (set-cdr! ~name (my-delay
                       ~body))
      ~name)))


(ann ^:no-check partial-sums (IFn [(FiniteStream Int) -> (FiniteStream Int)]
                                  [(FiniteStream Num) -> (FiniteStream Num)]
                                  [(InfiniteStream Int) -> (InfiniteStream Int)]
                                  [(InfiniteStream Num) -> (InfiniteStream Num)]))
(defn partial-sums
  "Q. 3.55"
  {:test #(is (= (to-list (stream-take (partial-sums integers) 5))
                 [1 3 6 10 15]))}
  [s]
  (def-stream ret (stream-car s)
    (add-streams ret
                 (stream-cdr s))))


(ann merge-streams
     (IFn [nil nil -> nil]

          [(Option (FiniteStream Int)) (FiniteStream Int) -> (FiniteStream Int)]
          [(FiniteStream Int) (Option (FiniteStream Int)) -> (FiniteStream Int)]
          [(Option (Stream Int)) (InfiniteStream Int) -> (InfiniteStream Int)]
          [(InfiniteStream Int) (Option (Stream Int)) -> (InfiniteStream Int)]

          ; todo: delete me
          [(Option (FiniteStream Int)) (Option (FiniteStream Int)) -> (Option (FiniteStream Int))]

          [(Option (FiniteStream Num)) (FiniteStream Num) -> (FiniteStream Num)]
          [(FiniteStream Num) (Option (FiniteStream Num)) -> (FiniteStream Num)]
          [(Option (Stream Num)) (InfiniteStream Num) -> (InfiniteStream Num)]
          [(InfiniteStream Num) (Option (Stream Num)) -> (InfiniteStream Num)]

          ; todo: delete me
          [(Option (FiniteStream Num)) (Option (FiniteStream Num)) -> (Option (FiniteStream Num))]))
(defn merge-streams
  "Q. 3.56"
  {:test #(let [s (cons-stream 1 (merge-streams
                                  (scale-stream integers 2)
                                  (merge-streams
                                   (scale-stream integers 3)
                                   (scale-stream integers 5))))]
            (is (= (to-list (stream-take s 10))
                   [1 2 3 4 5 6 8 9 10 12])))}
  [s1 s2]
  (cond
    (stream-null? s1) s2
    (stream-null? s2) s1
    :else
    (let [s1car (stream-car s1)
          s2car (stream-car s2)]
      (cond
        (< s1car s2car)
        (cons-stream s1car (merge-streams (stream-cdr s1) s2))
        (> s1car s2car)
        (cons-stream s2car (merge-streams s1 (stream-cdr s2)))
        :else
        (cons-stream s1car (merge-streams (stream-cdr s1)
                                          (stream-cdr s2)))))))


(ann fibs (InfiniteStream Int))
(def fibs (cons-stream 0
                       (cons-stream 1
                                    (add-streams (stream-cdr fibs)
                                                 fibs))))

; Q. 3.57 (max (- n 2) 0)


(ann expand [Int Int Int -> (InfiniteStream Int)])
(defn expand
  "Q. 3.58 (num/den)_radix"
  [num den radix]
  (cons-stream
   (quot (* num radix) den)
   (expand (rem (* num radix) den) den radix)))


(ann integrate-series (IFn [(FiniteStream Num) -> (FiniteStream Num)]
                           [(InfiniteStream Num) -> (InfiniteStream Num)]))
(defn integrate-series
  "Q. 3.59-a"
  [s]
  (stream-map (typed/fn [a :- Num k :- Num] (/ a k)) s integers))


(ann exp-series (InfiniteStream Num))
(def exp-series (cons-stream 1 (integrate-series exp-series)))


; Q 3.59-b
(declare sine-series)
(ann cosine-series (InfiniteStream Num))
(def cosine-series (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))


(ann sine-series (InfiniteStream Num))
(def sine-series (cons-stream 0 (integrate-series cosine-series)))


(ann concat-streams
     (All [a]
          (IFn [nil nil -> nil]
               [(Option (FiniteStream a)) (FiniteStream a) -> (FiniteStream a)]
               [(FiniteStream a) (Option (FiniteStream a)) -> (FiniteStream a)]
               [(Option (FiniteStream a)) (Option (FiniteStream a)) -> (Option (FiniteStream a))] ; todo: remove me
               [(Option (Stream a)) (InfiniteStream a) -> (InfiniteStream a)]
               [(InfiniteStream a) (Option (Stream a)) -> (InfiniteStream a)]
               )))
(defn concat-streams [s1 s2]
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (concat-streams (stream-cdr s1)
                                 s2))))


(ann stream-repeat (All [a] [a -> (InfiniteStream a)]))
(defn stream-repeat [x]
  (cons-stream x (stream-repeat x)))


(ann mul-series
     (IFn [(Option (Stream Int)) (Option (Stream Int)) -> (InfiniteStream Int)]
          [(Option (Stream Num)) (Option (Stream Num)) -> (InfiniteStream Num)]))
(defn mul-series
  "Q. 3.60"
  {:test #(is (= (to-list
                  (stream-take
                   (add-streams
                    (mul-series cosine-series
                                cosine-series)
                    (mul-series sine-series
                                sine-series))
                   10))
                 [1 0 0 0 0 0 0 0 0 0]))}
  [s1 s2]
  (letfn> [impl :- (IFn [(InfiniteStream Int) (InfiniteStream Int) -> (InfiniteStream Int)]
                        [(InfiniteStream Num) (InfiniteStream Num)-> (InfiniteStream Num)])
           (impl [s1 s2]
                 (let [s1car (stream-car s1)
                       s2car (stream-car s2)]
                   (cons-stream (* s1car
                                   s2car)
                                (let [s1cdr (stream-cdr s1)
                                      s2cdr (stream-cdr s2)]
                                  (add-streams (add-streams (scale-stream s1cdr s2car)
                                                            (scale-stream s2cdr s1car))
                                               (cons-stream 0
                                                            (mul-series s1cdr s2cdr)))))))]
    (impl (concat-streams s1 (stream-repeat 0))
          (concat-streams s2 (stream-repeat 0)))))


(ann ^:no-check invert-unit-series [(Stream Num) -> (InfiniteStream Num)])
(defn invert-unit-series
  "Q. 3.61"
  {:test #(is (= (to-list (stream-take (invert-unit-series (make-stream 1 2 3)) 5))
                 [1 -2 1 4 -11]))}
  [s]
  (def-stream ret 1
    (stream-cdr
     (cons-stream
      1
      (scale-stream
       (mul-series (stream-cdr s) ret)
       -1)))))


(ann div-series [(Stream Num) (Stream Num) -> (InfiniteStream Num)])
(defn div-series
  "Q. 3.62"
  {:test #(is (= (to-list
                  (stream-take (div-series sine-series cosine-series) 12))
                 [0 1 0 1/3 0 2/15 0 17/315 0 62/2835 0 1382/155925]))}
  [num den]
  (let [den-0 (stream-car den)]
    (if (zero? den-0)
      (throw (Exception. (str "den-0 should not be 0: " den)))
      (let [inv-den-0 (/ 1 den-0)]
        (scale-stream
         (mul-series num
                     (invert-unit-series
                      (scale-stream den inv-den-0)))
         inv-den-0)))))


(ann square (IFn [Int -> Int]
                 [Num -> Num]))
(defn square [x]
  (* x x))


(ann average [Num Num -> Num])
(defn average [x y]
  (/ (+ x y) 2))


(ann sqrt-improve [Num Num -> Num])
(defn sqrt-improve [guess x]
  (average guess (/ x guess)))


(ann ^:no-check sqrt-stream [Num -> (InfiniteStream Num)])
(defn sqrt-stream [x]
  (def-stream ret 1
    (stream-map (typed/fn [guess :- Num] (sqrt-improve guess x))
                ret)))


(ann pi-summands [Int -> (InfiniteStream Num)])
(defn pi-summands [n]
  (cons-stream (/ 1 n)
               (stream-map - (pi-summands (+ n 2)))))


(ann pi-stream (InfiniteStream Num))
(def pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))


(ann euler-transform [(InfiniteStream Num) -> (InfiniteStream Num)])
(defn euler-transform [s]
  (let [s0 (stream-ref s 0)
        s1 (stream-ref s 1)
        s2 (stream-ref s 2)]
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))


(ann make-tableau [[(InfiniteStream Num) -> (InfiniteStream Num)]
                   (InfiniteStream Num)
                   ->
                   (InfiniteStream (InfiniteStream Num))])
(defn make-tableau [transform s]
  (cons-stream s
               (make-tableau transform
                             (transform s))))


(ann ^:no-check accelerated-sequence [[(InfiniteStream Num) -> (InfiniteStream Num)]
                                      (InfiniteStream Num)
                                      ->
                                      (InfiniteStream Num)])
(defn accelerated-sequence [transform s]
  (stream-map stream-car
              (make-tableau transform s)))


; Q. 3.63 No


(ann stream-limit [(InfiniteStream Num) Num -> Num])
(defn stream-limit
  "Q. 3.64"
  [s tol]
  (let [s1 (stream-ref s 0)
        s2 (stream-ref s 1)]
    (if (< (abs (- s1 s2)) tol)
      s2
      (recur (stream-cdr s) tol))))


(ann ln2-summands [Int -> (InfiniteStream Num)])
(defn ln2-summands [k]
  (cons-stream (/ 1 k)
               (stream-map - (ln2-summands (inc k)))))


(ann l2-stream (InfiniteStream Num))
(def ln2-stream (partial-sums (ln2-summands 1)))


(ann q-3-51 [-> nil])
(defn q-3-65
  "Q. 3.65"
  []
  (let [diff-ln2 (typed/fn [s :- (InfiniteStream Num)]
                   (display-stream
                    (stream-take
                     (stream-map
                      (typed/fn [x :- Num] (- (Math/log 2) x))
                      s)
                     8))
                   (println :done))]
    (diff-ln2 ln2-stream)
    (diff-ln2 (euler-transform ln2-stream))
    (diff-ln2 (accelerated-sequence euler-transform ln2-stream))
    nil))
;(q-3-65)




(ann stream-interleave (All [a] [(InfiniteStream a) (InfiniteStream a) -> (InfiniteStream a)]))
(defn stream-interleave [s1 s2]
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (stream-interleave s2 (stream-cdr s1)))))


(ann pairs (All [a b] [(InfiniteStream a) (InfiniteStream b) -> (InfiniteStream (Pair a (Pair b nil)))]))
(defn pairs [s t]
  (cons-stream
   (my-cons
    (stream-car s)
    (my-cons
     (stream-car t)
     nil))
   (stream-interleave
    (stream-map (typed/fn [x :- b] (my-cons
                                    (stream-car s)
                                    (my-cons
                                     x
                                     nil)))
                (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))


; Q. 3.66
; (1 . 100) 198
; (99 . 100) 2^99 + 2^98
; (100 . 100) 2^100 - 1


(ann pairs-3-67 (All [a b] [(InfiniteStream a) (InfiniteStream b) -> (InfiniteStream (Pair a (Pair b nil)))]))
(defn pairs-3-67
  "Q. 3.67"
  [s t]
  (let [s0 (stream-car s)
        t0 (stream-car t)]
    (cons-stream
     (my-cons
      s0
      (my-cons
       t0
       nil))
     (let [s- (stream-cdr s)
           t- (stream-cdr t)]
       (stream-interleave
        (stream-interleave
         (stream-map (typed/fn [x :- b] (my-cons s0 (my-cons x nil))) t-)
         (stream-map (typed/fn [x :- a] (my-cons x (my-cons t0 nil))) s-))
        (pairs-3-67 s- t-))))))


; Q. 3.68 infinite recursive call
;; (ann pairs-3-68 (All [a b] [(InfiniteStream a) (InfiniteStream b) -> (InfiniteStream (Pair a b))]))
;; (defn pairs-3-68
;;   [s t]
;;   (stream-interleave
;;    (stream-map (typed/fn [x :- b] (my-cons (stream-car s) x))
;;                t)
;;    (pairs-3-68 (stream-cdr s)
;;                (stream-cdr t))))


(ann triples (All [a b c]
                  [(InfiniteStream a) (InfiniteStream b) (InfiniteStream c)
                   ->
                   (InfiniteStream (Pair a (Pair b (Pair c nil))))]))
(defn triples
  "Q. 3.69"
  [s t u]
  (let [s0tu (stream-map (typed/fn [tu :- (Pair b (Pair c nil))]
                          (my-cons (stream-car s)
                                   tu))
                        (pairs t u))]
    (cons-stream
     (stream-car s0tu)
     (stream-interleave
      (stream-cdr s0tu)
      (triples (stream-cdr s)
               (stream-cdr t)
               (stream-cdr u))))))


(ann pythagoras-triples (InfiniteStream (Pair Int (Pair Int (Pair Int nil)))))
(def pythagoras-triples (stream-filter
                         (typed/fn [l :- (Pair Int (Pair Int (Pair Int nil)))]
                           (= (+ (square (car l))
                                 (square (cadr l)))
                              (square (car (cddr l)))))
                         (triples integers integers integers)))


