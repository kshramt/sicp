(ns sicp.stream
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [All
            Any
            ASeq
            CountRange
            ExactCount
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
            any?
            car
            cdr
            my-cons
            my-list
            pair?
            set-car!
            set-cdr!
            ]
    ]
   [sicp.util
    :refer [
            p_
            pef
            sqrt
            ]]
   )
  (:import [sicp.pair Pair]))



(defalias Stream (TFn [[a :variance :covariant]] (Rec [this] (Pair a [-> (Option this)]))))


(ann ^:no-check memo-proc (All [a] [[-> a] -> [-> a]]))
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


(ann ^:no-check my-cons-stream (All [a] [a Any -> (Stream a)]))
(defn my-cons-stream [a b]
  (my-cons a b))


(defmacro cons-stream [a b]
  `(my-cons-stream ~a (my-delay ~b)))


(ann ^:no-check stream-null? (All [a] [(Option (Stream a)) -> Boolean
                                       :filters
                                       {:then (! (Stream a) 0)
                                        :else (is (Stream a) 0)}]))
(def stream-null? nil?)


(ann ^:no-check the-empty-stream nil)
(def the-empty-stream nil)


(ann my-force (All [a] [[-> a] -> a]))
(defn my-force [delayed-object]
  (delayed-object))


(ann ^:no-check stream-car (All [a] [(Stream a) -> a]))
(def stream-car car)


(ann ^:no-check stream-cdr (All [a] [(Stream a) -> (Option (Stream a))]))
(def stream-cdr (comp my-force cdr))


(ann stream-ref (All [a] [(Stream a) Int -> a]))
(defn stream-ref [s n]
  (if (= n 0)
    (stream-car s)
    (let [more (stream-cdr s)]
      (if (stream-null? more)
        (throw (Exception. (str "out of bound: " n)))
        (recur more (dec n))))))


(ann ^:no-check make-stream (All [a] [a a * -> (Stream a)]))
(defn make-stream [& xs]
  (let [impl (typed/fn imp [s :- (Seqable a)]
               (if-let [s (seq s)]
                 (cons-stream (first s)
                              (imp (rest s)))
                 nil))]
    (impl xs)))


(ann ^:no-check to-list (All [a] (IFn [nil -> nil]
                                      [(Stream a) -> (ASeq a)])))
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


(ann ^:no-check stream-map (All [c a b ...] [[a b ... b -> c] (Stream a) (Stream b) ... b -> (Stream c)]))
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


(ann stream-for-each (All [a b] [[a -> b] (Option (Stream a)) -> (Val :done)]))
(defn stream-for-each [proc s]
  (if (stream-null? s)
    :done
    (do (proc (stream-car s))
        (recur proc (stream-cdr s)))))


(ann display-stream (All [a] [(Stream a) -> (Val :done)]))
(defn display-stream [stream]
  (stream-for-each println stream))


(ann stream-enumerate-interval [Int Int -> (Option (Stream Int))])
(defn stream-enumerate-interval [lo hi]
  (if (> lo hi)
    the-empty-stream
    (cons-stream
     lo
     (stream-enumerate-interval (inc lo) hi))))


(ann ^:no-check stream-filter (All [a] (IFn [[a -> Boolean] nil -> nil]
                                            [[a -> Boolean] (Stream a) -> (Stream a)])))
(defn stream-filter [pred stream]
  (cond
    (stream-null? stream) the-empty-stream
    (pred (stream-car stream)) (cons-stream (stream-car stream)
                                            (stream-filter pred
                                                           (stream-cdr stream)))
    :else (recur pred (stream-cdr stream))))


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


(ann integers-starting-from [Int -> (Stream Int)])
(defn integers-starting-from [n]
  (cons-stream n (integers-starting-from (inc n))))


(ann integers (Stream Int))
(def integers (integers-starting-from 1))


(ann divisible? [Int Int -> Boolean])
(defn divisible? [x y]
  (zero? (rem x y)))


(ann sieve [(Stream Int) -> (Stream Int)])
(defn sieve [stream]
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (typed/fn [n :- Int]
             (not (divisible? n (stream-car stream))))
           (ignore-with-unchecked-cast
            (stream-cdr stream)
            (Stream Int))))))


(ann primes (Stream Int))
(def primes (sieve (integers-starting-from 2)))


; Q. 3.53 2^n


(ann add-streams (IFn [(Stream Int) (Stream Int) -> (Stream Int)]
                      [(Stream Num) (Stream Num) -> (Stream Num)]))
(defn add-streams [s1 s2]
  (stream-map + s1 s2))


(ann mul-streams (IFn [(Stream Int) (Stream Int) -> (Stream Int)]
                      [(Stream Num) (Stream Num) -> (Stream Num)]))
(defn mul-streams [s1 s2]
  (stream-map * s1 s2))


(ann scale-stream (IFn [(Stream Int) Int -> (Stream Int)]
                       [(Stream Num) Num -> (Stream Num)]))
(defn scale-stream [stream factor]
  (stream-map (partial * factor) stream))


(ann stream-take (All [a] (IFn [(Option (Stream a)) Int -> (Option (Stream a))]
                               [(Option (Stream a)) Int Int -> (Option (Stream a))])))
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
(ann factorials (Stream Int))
(def factorials (cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))


(defmacro -partial-sums [s]
  `(let [s# ~s]
     (var-get
      (def name#
         (cons-stream
          (stream-car s#)
          (add-streams name# (stream-cdr s#)))))))


(ann ^:no-check partial-sums (IFn [(Stream Int) -> (Stream Int)]
                                  [(Stream Num) -> (Stream Num)]))
(defn partial-sums
  "Q. 3.55"
  {:test #(is (= (to-list (stream-take (partial-sums integers) 5))
                 [1 3 6 10 15]))}
  [s]
  (-partial-sums s))


(ann ^:no-check merge-streams
     (IFn [(Option (Stream Int)) (Option (Stream Int)) -> (Option (Stream Int))]
          [(Option (Stream Num)) (Option (Stream Num)) -> (Option (Stream Num))]))
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


(ann fibs (Stream Int))
(def fibs (cons-stream 0
                       (cons-stream 1
                                    (add-streams (ignore-with-unchecked-cast
                                                  (stream-cdr fibs)
                                                  (Stream Int))
                                                 fibs))))

; Q. 3.57 (max (- n 2) 0)


(ann expand [Int Int Int -> (Stream Int)])
(defn expand
  "Q. 3.58 (num/den)_radix"
  [num den radix]
  (cons-stream
   (quot (* num radix) den)
   (expand (rem (* num radix) den) den radix)))


(ann integrate-series [(Stream Num) -> (Stream Num)])
(defn integrate-series
  "Q. 3.59-a"
  [s]
  (stream-map (typed/fn [a :- Num k :- Num] (/ a k)) s integers))


(ann exp-series (Stream Num))
(def exp-series (cons-stream 1 (integrate-series exp-series)))


; Q 3.59-b
(declare sine-series)
(ann cosine-series (Stream Num))
(def cosine-series (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
