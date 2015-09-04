(ns sicp.stream
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [All
            Any
            IFn
            Int
            Kw
            Num
            Option
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
   ))


(typed/tc-ignore
(def stream-null? nil?)


(def the-empty-stream nil)


(ann memo-proc (All [a] [[-> a] -> [-> a]]))
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


(defn my-force [delayed-object]
  (delayed-object))


;(ann cons-stream (All [a] [a (Seqable a) -> (Stream a)]))
(defmacro cons-stream [a b]
  `(my-cons ~a (my-delay ~b)))


(ann stream-car (All [a] [(Stream a) -> a]))
(def stream-car car)


(ann stream-cdr (All [a] [(Stream a) -> a]))
(def stream-cdr (comp my-force cdr))


(ann stream-ref (All [a] [(Stream a) Int -> a]))
(defn stream-ref [s n]
  (if (= n 0)
    (stream-car s)
    (recur (stream-cdr s) (dec n))))


(defn make-stream [& xs]
  (letfn [(impl [s]
            (if-let [s (seq s)]
              (cons-stream (first s)
                           (impl (rest s)))))]
    (impl xs)))


(defn to-list [stream]
  (if (stream-null? stream)
    nil
    (cons (stream-car stream)
          (to-list (stream-cdr stream)))))


(ann stream-map (All [c a b ...] [[a b ... b -> c] (Stream a) (Stream b) ... b -> (Stream c)]))
(defn stream-map
  "Q. 3.50"
  {:test #(is (to-list
               (stream-map
                +
                (make-stream 1 2 3)
                (make-stream 9 8 7)))
              [10 10 10])}
  [proc & arguments]
  (if (stream-null? (first arguments))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car arguments))
      (apply stream-map
             (cons proc (map stream-cdr arguments))))))


(ann stream-for-each (All [a b] [[a -> b] (Stream a) -> (Val :done)]))
(defn stream-for-each [proc s]
  (if (stream-null? s)
    :done
    (do (proc (stream-car s))
        (recur proc (stream-cdr s)))))


(ann display-stream (All [a] [(Stream a) -> (Val :done)]))
(defn display-stream [stream]
  (stream-for-each println stream))


(ann stream-filter [Int Int -> (Stream Int)])
(defn stream-enumerate-interval [lo hi]
  (if (> lo hi)
    the-empty-stream
    (cons-stream
     lo
     (stream-enumerate-interval (inc lo) hi))))


(ann stream-filter (All [a] [[a -> Boolean] (Stream a) -> (Stream a)]))
(defn stream-filter [pred stream]
  (cond
    (stream-null? stream) the-empty-stream
    (pred (stream-car stream)) (cons-stream (stream-car stream)
                                            (stream-filter pred
                                                           (stream-cdr stream)))
    :else (recur pred (stream-cdr stream))))


(defn q-3-51
  "Q. 3.51"
  []
  (let [x (stream-map println (stream-enumerate-interval 0 10))]
    (stream-ref x 5)
    (println "")
    (stream-ref x 7)))

; Q. 3.52 skip
) ; typed/tc-ignore
