(ns sicp.ch-2-4
  (:require [clojure.test :refer [is are deftest]]
            [clojure.pprint]
            [clojure.core.typed
             :refer
             [
              ann
              ann-form
              defalias
              letfn>
              Int Num
              Kw
              Sym
              Val
              Var1
              Pred
              Option
              Seqable
              NonEmptySeqable
              EmptySeqable
              ASeq
              Any
              All
              U
              IFn
              ] :as typed]
            [clojure.core.typed.unsafe
             :refer
             [
              ignore-with-unchecked-cast
              ]]
            [clojure.repl]))
(set! *warn-on-reflection* false)


(typed/override-method java.lang.Math/sin [Num -> Double])
(typed/override-method java.lang.Math/cos [Num -> Double])
(typed/override-method java.lang.Math/sqrt [Num -> Double])
(typed/override-method java.lang.Math/atan2 [Num Num -> Double])
 ; for inlined `rem`
(typed/override-method clojure.lang.Numbers/remainder (IFn [Int Int -> Int]
                                                           [Num Num -> Num]))


(ann ^:no-check clojure.test/test-var [(Var1 [-> nil]) -> nil])


(defalias LazySeq clojure.lang.LazySeq)


(defalias IntegerTag (Val :integer))
(defalias TaggedInteger '[IntegerTag Int])
(defalias RationalTag (Value :rational))
(defalias TaggedRationalInternal '[TaggedInteger TaggedInteger])
(defalias TaggedRational '[RationalTag TaggedRationalInternal])
(defalias RealTag (Val :real))
(defalias TaggedReal '[RealTag Num])
(defalias TaggedFloat (U TaggedInteger TaggedRational TaggedReal))
(defalias RectangularComplexTag (Val :rectangular))
(defalias TaggedRawComplexInternal '[TaggedFloat TaggedFloat])
(defalias TaggedRectangularComplex '[RectangularComplexTag TaggedRawComplexInternal])
(defalias PolarComplexTag (Val :polar))
(defalias TaggedPolarComplex '[PolarComplexTag TaggedRawComplexInternal])
(defalias ComplexTag (Val :complex))
(defalias TaggedRawComplex (U TaggedRectangularComplex
                              TaggedPolarComplex))
(defalias TaggedComplex '[ComplexTag TaggedRawComplex])
(defalias TaggedNumber (U TaggedFloat TaggedComplex))
(defalias TaggedObject (U Num Boolean '[Kw Any]))
(defalias ClojureNumberTag (Val :clojure-number))
(defalias BooleanTag (Val :boolean))
(defalias TermInternal '[TaggedInteger TaggedNumber])
(defalias TermTag (Val :term))
(defalias Term '[TermTag TermInternal])

(defalias DenseTermListTag (Val :dense-term-list))
(defalias NonEmptyDenseTermListInternal (NonEmptySeqable TaggedNumber))
(defalias EmptyDenseTermListInternal (EmptySeqable TaggedNumber))
(defalias DenseTermListInternal (U NonEmptyDenseTermListInternal
                                   EmptyDenseTermListInternal))
(defalias NonEmptyDenseTermList '[DenseTermListTag NonEmptyDenseTermListInternal])
(defalias EmptyDenseTermList '[DenseTermListTag EmptyDenseTermListInternal])
(defalias DenseTermList (U NonEmptyDenseTermList
                           EmptyDenseTermList))

(defalias SparseTermListTag (Val :sparse-term-list))
(defalias NonEmptySparseTermListInternal (NonEmptySeqable TermInternal))
(defalias EmptySparseTermListInternal (EmptySeqable TermInternal))
(defalias SparseTermListInternal (U NonEmptySparseTermListInternal
                                    EmptySparseTermListInternal))
(defalias NonEmptySparseTermList '[SparseTermListTag NonEmptySparseTermListInternal])
(defalias EmptySparseTermList '[SparseTermListTag EmptySparseTermListInternal])
(defalias SparseTermList (U NonEmptySparseTermList
                            EmptySparseTermList))

(defalias NonEmptyTermList (U NonEmptyDenseTermList
                              NonEmptySparseTermList))
(defalias EmptyTermList (U EmptyDenseTermList
                           EmptySparseTermList))
(defalias TermList (U DenseTermList
                      SparseTermList))


(defmacro p- [x]
  `(let [x# ~x]
     (println ~(str &form))
     (clojure.pprint/pprint x#)
     x#))


(defmacro pef
  "print-env-form"
  [form]
  `(let [RETURN# ~form
         _# (typed/print-env ~(str form))]
     RETURN#))


(ann ^:no-check attach-tag
     (IFn [ClojureNumberTag Int -> Int]
          [ClojureNumberTag Num -> Num]
          [BooleanTag Boolean -> Boolean]
          [IntegerTag Int -> TaggedInteger]
          [RationalTag TaggedRationalInternal -> TaggedRational]
          [RealTag Num -> TaggedReal]
          [RectangularComplexTag TaggedRawComplexInternal -> TaggedRectangularComplex]
          [PolarComplexTag TaggedRawComplexInternal -> TaggedPolarComplex]
          [ComplexTag TaggedRawComplex -> TaggedComplex]
          [TermTag TermInternal -> Term]
          [DenseTermListTag NonEmptyDenseTermListInternal -> NonEmptyDenseTermList]
          [DenseTermListTag EmptyDenseTermListInternal -> EmptyDenseTermList]
          [SparseTermListTag NonEmptySparseTermListInternal -> NonEmptySparseTermList]
          [SparseTermListTag EmptySparseTermListInternal -> EmptySparseTermList]))
(defn attach-tag [type-tag contents]
  (case type-tag
    :clojure-number contents
    :boolean contents
    [type-tag contents]))


(ann ^:no-check type-tag
     (IFn [Num -> ClojureNumberTag]
          [Boolean -> BooleanTag]
          [TaggedInteger -> IntegerTag]
          [TaggedRational -> RationalTag]
          [TaggedReal -> RealTag]
          [TaggedRectangularComplex -> RectangularComplexTag]
          [TaggedPolarComplex -> PolarComplexTag]
          [TaggedComplex -> ComplexTag]))
(defn type-tag [datum]
  (cond
   (number? datum) :clojure-number
   (instance? Boolean datum) :boolean
   :else (first datum)))


(ann ^:no-check contents
     (IFn [Int -> Int]
          [Num -> Num]
          [Boolean -> Boolean]
          [TaggedInteger -> Int]
          [TaggedRational -> TaggedRationalInternal]
          [TaggedReal -> Num]
          [TaggedRectangularComplex -> TaggedRawComplexInternal]
          [TaggedPolarComplex -> TaggedRawComplexInternal]
          [TaggedComplex -> TaggedRawComplex]))
(defn contents [datum]
  (cond
    (number? datum) datum
    (instance? Boolean datum) datum
    :else (second datum)))


(ann lookup (All [a b c
                  d e f g h]
                 (IFn [a (Seqable '[b c]) -> (Option c)]
                      [d e (Seqable '[f (Seqable '[g h])]) -> (Option h)])))
(defn lookup
  ([key table]
     (if-let [[k v] (first table)]
       (if (= k key)
         v
         (recur key (rest table)))))
  ([key-1 key-2 table]
     (if-let [inner-table (lookup key-1 table)]
       (lookup key-2 inner-table))))


(ann ^:no-check insert
     (All [a b c d
           e f g h i j]
          (IFn [a b (Seqable '[c d]) -> (ASeq (U '[a b] '[c d]))]
               [e f g (Seqable '[h (Seqable '[i j])])
                -> (ASeq (U '[e (ASeq '[f g])]
                            '[h (ASeq (U '[f g]
                                         '[i j]))]))])))
(defn insert
  ([key value table]
     (if-let [[k v :as kv] (first table)]
       (if (= k key)
         (cons [k value] (rest table))
         (cons kv (insert key value (rest table))))
       (cons [key value] nil)))
  ([key-1 key-2 value table]
     (if-let [inner-table (lookup key-1 table)]
        (insert key-1 (insert key-2 value inner-table) table)
        (insert key-1 (insert key-2 value []) table))))


(ann abs- (All [[a :< Num]] [a -> a]))
(defn abs- [x] (if (pos? x) x (ignore-with-unchecked-cast (- x) a)))


(ann ^:no-check make-table
     [-> (IFn
          [(Val :lookup)
           -> [Kw (U Kw (Seqable Kw)) -> Any]]
          [(Val :insert!)
           -> [Kw (U Kw (Seqable Kw)) Any
               -> (LazySeq
                   '[Kw
                     (Seqable
                      '[(U Kw
                           (Seqable Kw))
                        Any])])]])])
(defn make-table
  {:test #(do (is (let [t (make-table)]
                    ((t :insert!) :a :b 1)
                    (= ((t :lookup) :a :b) 1)))
              (is (let [t (make-table)]
                    ((t :insert!) :a :b 1)
                    (nil? ((t :lookup) :b :a))))
              (is (let [t (make-table)]
                    ((t :insert!) :a :b 1)
                    ((t :insert!) :b :a 2)
                    (= ((t :lookup) :b :a) 2))))}
  []
  (let [local-table (atom [])
        dispatch (fn [method]
                   (case method
                     :lookup (fn [key-1 key-2] (lookup key-1 key-2 @local-table))
                     :insert! (fn [key-1 key-2 value] (reset! local-table (insert key-1 key-2 value @local-table)))
                     (throw (Exception. (str "unknown method:  " method)))))]
    dispatch))

(ann operation-table (IFn
                      [(Val :lookup)
                       -> [Kw (U Kw (Seqable Kw)) -> Any]]
                      [(Val :insert!)
                       -> [Kw (U Kw (Seqable Kw)) Any
                           -> (LazySeq
                               '[Kw
                                 (Seqable
                                  '[(U Kw
                                       (Seqable Kw))
                                    Any])])]]))
(def operation-table (make-table))


(ann get_ [Kw (U Kw (Seqable Kw)) -> Any])
(def get_ (operation-table :lookup))


(ann put [Kw (U Kw (Seqable Kw)) Any
          -> (LazySeq
              '[Kw
                (Seqable
                 '[(U Kw
                      (Seqable Kw))
                   Any])])])
(def put (operation-table :insert!))


(ann ^:no-check apply-generic-basic [Kw TaggedObject * -> TaggedObject])
(defn apply-generic-basic [op & args]
  (let [type-tags (map type-tag args)]
    (if-let [proc (get_ op type-tags)]
      (apply proc (map contents args))
      (throw (Exception. (str "no mtehod for these types: " op " " (apply list type-tags)))))))


(declare equ? raise)


(ann ^:no-check projectable?
     (IFn [TaggedComplex -> (Option [TaggedRawComplex -> TaggedReal])]
          [TaggedReal -> (Option [Num -> TaggedRational])]
          [TaggedRational -> (Option [TaggedRawComplexInternal -> TaggedInteger])]
          [TaggedObject -> nil]))
(defn- projectable? [x]
  (get_ :project [(type-tag x)]))


(ann ^:no-check drop_ ; xxx:
     (IFn [TaggedComplex -> TaggedNumber]
          [TaggedReal -> TaggedFloat]
          [TaggedRational -> (U TaggedInteger TaggedRational)]
          [TaggedInteger -> TaggedInteger]
          [Int -> Int]
          [Num -> Num]
          [Boolean -> Boolean]
          [TaggedObject -> TaggedObject]))
(defn drop_ [x]
  (if-let [project- (projectable? x)]
    (let [xdown (project- (contents x))]
      (if (equ? (raise xdown) x)
        (recur xdown)
        x))
    x))


(ann ^:no-check raisable?
     (IFn [TaggedInteger -> (Option [Int -> TaggedRational])]
          [TaggedRational -> (Option  [TaggedRationalInternal -> TaggedReal])]
          [TaggedReal -> (Option [Num -> TaggedComplex])]
          [TaggedObject -> nil]))
(defn- raisable? [x]
  (get_ :raise [(type-tag x)]))


(ann raised-list (IFn [TaggedInteger -> (ASeq (U TaggedInteger TaggedRational TaggedReal TaggedComplex))]
                      [TaggedRational -> (ASeq (U TaggedRational TaggedReal TaggedComplex))]
                      [TaggedReal -> (ASeq (U TaggedReal TaggedComplex))]
                      [TaggedComplex -> (ASeq TaggedComplex)]
                      [TaggedObject -> (ASeq TaggedObject)]))
(defn- raised-list [x]
  (if-let [raise- (raisable? x)]
    (cons x (raised-list (raise- (contents x))))
    (cons x nil)))


(ann index (All [a b] (IFn [a (Seqable b) -> (Option Int)]
                           [a (Seqable b) Int -> (Option Int)])))
(defn- index
  {:test #(do (are [y xs i] (= (index y xs) i)
                   1 [1 2 3] 0
                   1 [2 1 3] 1
                   0 [1 2 3] nil
                   1 [] nil))}
  ([y xs] (index y xs 0))
  ([y xs i]
     (when-let [s (seq xs)]
       (if (= (first s) y)
         i
         (recur y (rest s) (inc i))))))


(ann ^:no-check raise-up-to
     (IFn [TaggedInteger IntegerTag -> TaggedInteger]
          [TaggedInteger RationalTag -> TaggedRational]
          [TaggedInteger RealTag -> TaggedReal]
          [TaggedInteger ComplexTag -> TaggedComplex]
          [TaggedRational RationalTag -> TaggedRational]
          [TaggedRational RealTag -> TaggedReal]
          [TaggedRational ComplexTag -> TaggedComplex]
          [TaggedReal RealTag -> TaggedReal]
          [TaggedReal ComplexTag -> TaggedComplex]
          [TaggedComplex ComplexTag -> TaggedComplex]))
(defn- raise-up-to [x t]
  (if (= (type-tag x) t)
    x
    (if-let [raise- (raisable? x)]
      (recur (raise- (contents x)) t)
      (throw (Exception. (str "Unable to raise " x " up to " t))))))


(ann ^:no-check apply-generic-2-84- [Kw (Option (Seqable TaggedObject)) -> TaggedObject])
(defn- apply-generic-2-84-
  "Q. 2.84"
  [op args]
  (let [type-tags (map type-tag args)
        error #(throw (Exception. (str "no method for these types: " op " " (apply list args))))]
    (if-let [proc (get_ op type-tags)]
      (apply proc (map contents args))
      (if (or (apply = type-tags) (< (count args) 2))
        (error)
        (let [raised-t1s (map type-tag (raised-list (first args)))
              t-heights (map (comp #(if (nil? %) -1 %)
                                   #(index % raised-t1s))
                             type-tags)
              t (nth raised-t1s (apply max t-heights))]
          (apply-generic-2-84- op (map #(raise-up-to % t) args)))))))


(ann apply-generic-2-84 [Kw TaggedObject * -> TaggedObject])
(defn apply-generic-2-84
  "Q. 2.84"
  [op & args]
  (apply-generic-2-84- op args))


(ann apply-generic-2-86 [Kw TaggedObject * -> TaggedObject])
(defn apply-generic-2-86
  "Q. 2.86"
  [op & args]
;  (println op args)
  (let [ret (apply-generic-2-84- op args)]
    (case op
        :raise ret
        :project ret
        :drop ret
        (drop_ ret))))


(def apply-generic apply-generic-2-86)


(ann coercion-table (IFn
                     [(Val :lookup)
                      -> [Kw (U Kw (Seqable Kw)) -> Any]]
                     [(Val :insert!)
                      -> [Kw (U Kw (Seqable Kw)) Any
                          -> (LazySeq
                              '[Kw
                                (Seqable
                                 '[(U Kw
                                      (Seqable Kw))
                                   Any])])]]))
(def coercion-table (make-table))


(ann get-coercion [Kw (U Kw (Seqable Kw)) -> Any])
(def get-coercion (coercion-table :lookup))


(ann put-coercion [Kw (U Kw (Seqable Kw)) Any
                   -> (LazySeq
                       '[Kw
                         (Seqable
                          '[(U Kw
                               (Seqable Kw))
                            Any])])])
(def put-coercion (coercion-table :insert!))


(ann ^:no-check real-part (IFn [TaggedComplex -> TaggedFloat]
                               [TaggedRawComplex -> TaggedFloat]))
(defn real-part [x] (apply-generic :real-part x))


(ann ^:no-check imag-part (IFn [TaggedComplex -> TaggedFloat]
                               [TaggedRawComplex -> TaggedFloat]))
(defn imag-part [x] (apply-generic :imag-part x))


(ann ^:no-check magnitude (IFn [TaggedComplex -> TaggedFloat]
                               [TaggedRawComplex -> TaggedFloat]))
(defn magnitude [x] (apply-generic :magnitude x))


(ann ^:no-check angle (IFn [TaggedComplex -> TaggedFloat]
                           [TaggedRawComplex -> TaggedFloat]))
(defn angle [x] (apply-generic :angle x))


(ann ^:no-check add (IFn [Int Int -> Int]
                         [Num Num -> Num]
                         [TaggedInteger TaggedInteger -> TaggedInteger]
                         [(U TaggedInteger TaggedRational) (U TaggedInteger TaggedRational) -> (U TaggedInteger TaggedRational)]
                         [TaggedFloat TaggedFloat -> TaggedFloat]
                         [TaggedNumber TaggedNumber -> TaggedNumber]))
(defn add [x y] (apply-generic :add x y))


(ann ^:no-check sub (IFn [Int Int -> Int]
                         [Num Num -> Num]
                         [TaggedInteger TaggedInteger -> TaggedInteger]
                         [(U TaggedInteger TaggedRational) (U TaggedInteger TaggedRational) -> (U TaggedInteger TaggedRational)]
                         [TaggedFloat TaggedFloat -> TaggedFloat]
                         [TaggedNumber TaggedNumber -> TaggedNumber]))
(defn sub [x y] (apply-generic :sub x y))


(ann ^:no-check mul (IFn [Int Int -> Int]
                         [Num Num -> Num]
                         [TaggedInteger TaggedInteger -> TaggedInteger]
                         [(U TaggedInteger TaggedRational) (U TaggedInteger TaggedRational) -> (U TaggedInteger TaggedRational)]
                         [TaggedFloat TaggedFloat -> TaggedFloat]
                         [TaggedNumber TaggedNumber -> TaggedNumber]))
(defn mul [x y] (apply-generic :mul x y))


(ann ^:no-check negate (IFn [Int -> Int]
                            [Num -> Num]
                            [TaggedInteger -> TaggedInteger]
                            [TaggedRational -> (U TaggedInteger TaggedRational)]
                            [TaggedReal -> TaggedFloat]
                            [TaggedRectangularComplex -> TaggedRectangularComplex]
                            [TaggedPolarComplex -> TaggedPolarComplex]
                            [TaggedComplex -> TaggedNumber]
                            [TaggedFloat -> TaggedFloat] ; xxx: realy necessary?
                            [TaggedNumber -> TaggedNumber] ; xxx: realy necessary?
                            [Term -> Term]
                            [EmptyDenseTermList -> EmptyDenseTermList]
                            [NonEmptyDenseTermList -> NonEmptyDenseTermList]
                            [EmptySparseTermList -> EmptySparseTermList]
                            [NonEmptySparseTermList -> NonEmptySparseTermList]
                            [EmptyTermList -> EmptyTermList]
                            [NonEmptyTermList -> NonEmptyTermList]
                            [TermList -> TermList]
                            ))
(defn negate [x] (apply-generic :negate x))


(ann ^:no-check div (IFn [Num Num -> Num]
                         [(U TaggedInteger TaggedRational) (U TaggedInteger TaggedRational) -> (U TaggedInteger TaggedRational)]
                         [TaggedFloat TaggedFloat -> TaggedFloat]
                         [TaggedNumber TaggedNumber -> TaggedNumber]))
(defn div [x y] (apply-generic :div x y))


(ann ^:no-check div-truncate (IFn [Num -> Int]
                                  [(U TaggedInteger TaggedRational) (U TaggedInteger TaggedRational) -> TaggedInteger]))
(defn div-truncate [x y] (apply-generic :div-truncate x y))


(ann ^:no-check sin (IFn [Num -> Num]
                         [TaggedFloat -> TaggedFloat]
                         [TaggedComplex -> TaggedNumber]))
(defn sin [x] (apply-generic :sin x))


(ann ^:no-check cos (IFn [Num -> Num]
                         [TaggedFloat -> TaggedFloat]
                         [TaggedComplex -> TaggedNumber]))
(defn cos [x] (apply-generic :cos x))


(ann ^:no-check sqrt (IFn [Num -> Num]
                          [TaggedFloat -> TaggedFloat]
                          [TaggedComplex -> TaggedNumber]))
(defn sqrt [x] (apply-generic :sqrt x))


(ann ^:no-check abs (IFn [Int -> Int]
                         [Num -> Num]
                         [TaggedInteger -> TaggedInteger]
                         [TaggedRational -> TaggedRational]
                         [TaggedReal -> TaggedReal]
                         [TaggedComplex -> TaggedFloat]))
(defn abs [x] (apply-generic :abs x))


(ann ^:no-check atan2 (IFn [Num Num -> Num]
                           [TaggedFloat TaggedFloat -> TaggedFloat]))
(defn atan2 [y x] (apply-generic :atan2 y x))


(ann ^:no-check lt? (IFn [Num Num -> Boolean]
                         [TaggedFloat TaggedFloat -> Boolean]))
(defn lt? [x y] (apply-generic :lt? x y))


(ann ^:no-check gt? (IFn [Num Num -> Boolean]
                         [TaggedFloat TaggedFloat -> Boolean]))
(defn gt? [x y] (apply-generic :gt? x y))


(ann ^:no-check rem_ (IFn [Int Int -> Int]
                          [TaggedInteger TaggedInteger -> TaggedInteger]))
(defn rem_ [x y] (apply-generic :rem_ x y))


(ann ^:no-check equ? (IFn [Num Num -> Boolean]
                          [TaggedNumber TaggedNumber -> Boolean]))
(defn equ? [x y] (apply-generic :equ? x y))


(ann ^:no-check =zero? [TaggedObject -> Boolean])
(defn =zero? [x] (apply-generic :=zero? x))


(ann ^:no-check raise (IFn [TaggedInteger -> TaggedRational]
                           [TaggedRational -> TaggedReal]
                           [TaggedReal -> TaggedComplex]))
(defn raise [x] (apply-generic :raise x))


(ann project [TaggedObject -> TaggedObject])
(defn project [x] (apply-generic :project x))


(ann square (IFn [Int -> Int]
                 [Num -> Num]
                 [TaggedInteger -> TaggedInteger]
                 [(U TaggedInteger TaggedRational) -> (U TaggedInteger TaggedRational)]
                 [TaggedFloat -> TaggedFloat]
                 [TaggedNumber -> TaggedNumber]))
(defn square [x] (mul x x))


(ann le? (IFn [Num Num -> Boolean]
              [TaggedFloat TaggedFloat -> Boolean]))
(defn le? [x y] (or (lt? x y) (equ? x y)))


(ann gcd (IFn [Int Int -> Int]
              [TaggedInteger TaggedInteger -> TaggedInteger]))
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
        small (if (lt? abs-m abs-n) abs-m abs-n)
        large (if (gt? abs-m abs-n) abs-m abs-n)]
    (if (=zero? small)
      large
      (recur (rem_ large small) small))))


; clojure number package
(ann install-clojure-number-package [-> (Val :done)])
(defn install-clojure-number-package []
  (letfn> [tag :- (IFn [Int -> Int] [Num -> Num])
           (tag [x] (attach-tag :clojure-number x))]
    (put :add [:clojure-number :clojure-number]
         (ann-form #(tag (+ %1 %2)) (IFn [Int Int -> Int] [Num Num -> Num])))
    (put :sub [:clojure-number :clojure-number]
         (ann-form #(tag (- %1 %2)) (IFn [Int Int -> Int] [Num Num -> Num])))
    (put :mul [:clojure-number :clojure-number]
         (ann-form #(tag (* %1 %2)) (IFn [Int Int -> Int] [Num Num -> Num])))
    (put :div [:clojure-number :clojure-number]
         (ann-form #(tag (/ %1 %2)) [Num Num -> Num]))
    (put :div-truncate [:clojure-number :clojure-number]
         (ann-form #(tag (int (div %1 %2))) [Num Num -> Int]))
    (put :negate [:clojure-number]
         (ann-form #(tag (* -1 %)) (IFn [Int -> Int] [Num -> Num])))
    (put :abs [:clojure-number]
         (ann-form #(tag (abs- %)) (IFn [Int -> Int] [Num -> Num])))
    (put :sin [:clojure-number] (ann-form #(tag (Math/sin %)) [Num -> Num]))
    (put :cos [:clojure-number] (ann-form #(tag (Math/cos %)) [Num -> Num]))
    (put :sqrt [:clojure-number] (ann-form #(tag (Math/sqrt %)) [Num -> Num]))
    (put :atan2 [:clojure-number :clojure-number]
         (ann-form #(tag (Math/atan2 %1 %2)) [Num Num -> Num]))
    (put :lt? [:clojure-number :clojure-number] <)
    (put :gt? [:clojure-number :clojure-number] >)
    (put :rem_ [:clojure-number :clojure-number]
         (ann-form #(tag (rem %1 %2)) [Int Int -> Int]))
    (put :equ? [:clojure-number :clojure-number] ==)
    (put :=zero? [:clojure-number] zero?)
    (put :make :clojure-number tag))
  :done)
(install-clojure-number-package)
(ann ^:no-check make-clojure-number (IFn [Int -> Int]
                                         [Num -> Num]))
(def make-clojure-number (get_ :make :clojure-number))


; integer package
(declare make-rational make-integer make-real)
(ann install-integer-package [-> (Val :done)])
(defn install-integer-package []
  (letfn> [tag :- [Int -> TaggedInteger]
           (tag [x] (attach-tag :integer x))]
    (put :add [:integer :integer] (typed/fn [x :- Int y :- Int] (tag (add x y))))
    (put :sub [:integer :integer] (typed/fn [x :- Int y :- Int] (tag (sub x y))))
    (put :mul [:integer :integer] (typed/fn [x :- Int y :- Int] (tag (mul x y))))
    (put :div [:integer :integer] (typed/fn [x :- Int y :- Int] (make-rational (tag x) (tag y))))
    (put :div-truncate [:integer :integer] (typed/fn [x :- Int y :- Int] (tag (int (div x y)))))
    ; `comp` cannot be applied here
    (put :abs [:integer] (comp tag abs))
    (put :negate [:integer] (comp tag negate))
    (put :sqrt [:integer] (typed/fn [x :- Int] (sqrt (raise (tag x)))))
    (put :cos [:integer] (typed/fn [x :- Int] (cos (raise (tag x)))))
    (put :sin [:integer] (typed/fn [x :- Int] (sin (raise (tag x)))))
    (put :lt? [:integer :integer] lt?)
    (put :gt? [:integer :integer] gt?)
    (put :rem_ [:integer :integer] (typed/fn [x :- Int y :- Int] (tag (rem_ x y))))
    (put :equ? [:integer :integer] equ?)
    (put :=zero? [:integer] =zero?)
    (put :raise [:integer] (typed/fn [x :- Int] (make-rational (tag x) (tag 1))))
    (put :make :integer (comp tag int)))
  :done)
(install-integer-package)
(ann ^:no-check make-integer [Num -> TaggedInteger])
(def make-integer (get_ :make :integer))


; rational package
(declare make-real)
(ann install-rational-package [-> (Val :done)])
(defn install-rational-package []
  (let [tag (typed/fn [x :- TaggedRationalInternal] (attach-tag :rational x))
        numer first
        denom second
        make-rat (typed/fn [n :- TaggedInteger d :- TaggedInteger]
                   (let [g (gcd n d)]
                     [(div-truncate n g) (div-truncate d g)]))]
    (put :add [:rational :rational] (typed/fn [x :- TaggedRationalInternal
                                               y :- TaggedRationalInternal]
                                      (tag (make-rat (add (mul (numer x)
                                                               (denom y))
                                                          (mul (denom x)
                                                               (numer y)))
                                                     (mul (denom x)
                                                          (denom y))))))
    (put :sub [:rational :rational] (typed/fn [x :- TaggedRationalInternal
                                               y :- TaggedRationalInternal]
                                      (tag (make-rat (sub (mul (numer x)
                                                               (denom y))
                                                          (mul (denom x)
                                                             (numer y)))
                                                     (mul (denom x)
                                                          (denom y))))))
    (put :mul [:rational :rational] (typed/fn [x :- TaggedRationalInternal
                                               y :- TaggedRationalInternal]
                                      (tag (make-rat (mul (numer x)
                                                          (numer y))
                                                     (mul (denom x)
                                                          (denom y))))))
    (put :div [:rational :rational] (typed/fn [x :- TaggedRationalInternal
                                               y :- TaggedRationalInternal]
                                      (tag (make-rat (mul (numer x)
                                                          (denom y))
                                                     (mul (denom x)
                                                          (numer y))))))
    (put :negate [:rational] (typed/fn [x :- TaggedRationalInternal]
                               (tag (make-rat (negate (numer x))
                                              (denom x)))))
    (put :abs [:rational] (typed/fn [x :- TaggedRationalInternal]
                            (tag (make-rat (abs (numer x))
                                           (abs (denom x))))))
    (put :sqrt [:rational] (typed/fn [x :- TaggedRationalInternal]
                             (sqrt (raise (tag x)))))
    (put :cos [:rational] (typed/fn [x :- TaggedRationalInternal]
                             (cos (raise (tag x)))))
    (put :sin [:rational] (typed/fn [x :- TaggedRationalInternal]
                             (sin (raise (tag x)))))
    (put :lt? [:rational :rational] (typed/fn [x :- TaggedRationalInternal
                                               y :- TaggedRationalInternal]
                                      (lt? (raise (tag x)) (raise (tag y)))))
    (put :gt? [:rational :rational] (typed/fn [x :- TaggedRationalInternal
                                               y :- TaggedRationalInternal]
                                      (gt? (raise (tag x)) (raise (tag y)))))
    (put :equ? [:rational :rational] (typed/fn [x :- TaggedRationalInternal
                                                y :- TaggedRationalInternal]
                                       (and (equ? (numer x)
                                                  (numer y))
                                            (equ? (denom x)
                                                  (denom y)))))
    (put :=zero? [:rational] (ignore-with-unchecked-cast
                              (comp =zero? numer)
                              [TaggedRationalInternal -> Boolean]))
    (put :raise [:rational] (typed/fn [x :- TaggedRationalInternal]
                              (make-real (div (contents (numer x))
                                              (contents (denom x))))))
    (put :project [:rational] (typed/fn [x :- TaggedRationalInternal]
                                (div-truncate (numer x) (denom x))))
    (put :make :rational (comp tag make-rat))
    )
  :done)
(install-rational-package)
(ann ^:no-check make-rational [TaggedInteger TaggedInteger -> TaggedRational])
(def make-rational (get_ :make :rational))


(ann real->rational [Num -> '[Int Int]])
(defn- real->rational
  {:test #(do (is (= (real->rational 3.2) [16 5]))
              (is (= (real->rational 0.25) [1 4])))}
  [x]
  (let [approx-equal (typed/fn [a :- Num b :- Num] (<= (abs- (- a b)) 1e-7))]
    (typed/loop [a :- Int 1
                 b :- Int 0
                 c :- Int (bigint x)
                 d :- Int 1
                 y :- Num x]
      (if (approx-equal (/ c a) x)
        [c a]
        (let [y (/ 1 (- y (bigint y)))
              iy (bigint y)]
          (recur (+ (* a iy) b)
                 a
                 (+ (* c iy) d)
                 c
                 y))))))


(declare make-complex-from-real-imag)
(ann install-real-package [-> (Val :done)])
(defn install-real-package []
  (let [tag (typed/fn [x :- Num] (attach-tag :real x))]
    (put :add [:real :real] (typed/fn [x :- Num y :- Num] (tag (add x y))))
    (put :sub [:real :real] (typed/fn [x :- Num y :- Num] (tag (sub x y))))
    (put :mul [:real :real] (typed/fn [x :- Num y :- Num] (tag (mul x y))))
    (put :div [:real :real] (typed/fn [x :- Num y :- Num] (tag (div x y))))
    (put :abs [:real] (comp tag abs))
    (put :negate [:real] (comp tag negate))
    (put :sin [:real] (comp tag sin))
    (put :cos [:real] (comp tag cos))
    (put :sqrt [:real] (comp tag sqrt))
    (put :atan2 [:real :real] (typed/fn [y :- Num x :- Num] (tag (atan2 y x))))
    (put :lt? [:real :real] lt?)
    (put :gt? [:real :real] gt?)
    (put :equ? [:real :real] equ?)
    (put :=zero? [:real] =zero?)
    (put :raise [:real] (typed/fn [x :- Num]
                          (make-complex-from-real-imag (tag x) (tag 0))))
    (put :project [:real] (typed/fn [x :- Num]
                            (let [[n d] (real->rational x)]
                              (make-rational (make-integer n)
                                             (make-integer d)))))
    (put :make :real (comp tag double)))
  :done)
(install-real-package)
(ann ^:no-check make-real [Num -> TaggedReal])
(def make-real (get_ :make :real))


(ann install-rectangular-package [-> (Val :done)])
(defn install-rectangular-package []
  (let [real-part first
        imag-part second
        make-from-real-imag (typed/fn [r :- TaggedFloat i :- TaggedFloat]
                              (attach-tag :rectangular [r i]))]
    (put :real-part [:rectangular] real-part)
    (put :imag-part [:rectangular] imag-part)
    (put :magnitude [:rectangular] (typed/fn [x :- TaggedRawComplexInternal]
                                     (sqrt (add (square (real-part x))
                                                (square (imag-part x))))))
    (put :angle [:rectangular] (typed/fn [x :- TaggedRawComplexInternal]
                                 (atan2 (imag-part x)
                                        (real-part x))))
    (put :make-from-real-imag :rectangular make-from-real-imag)
    (put :make-from-mag-ang :rectangular (typed/fn [x :- TaggedFloat y :- TaggedFloat]
                                           (make-from-real-imag (mul x (cos y))
                                                                (mul x (sin y))))))
  :done)
(install-rectangular-package)


(ann install-polar-package [-> (Val :done)])
(defn install-polar-package []
  (let [magnitude first
        angle second
        make-from-mag-ang (typed/fn [r :- TaggedFloat t :- TaggedFloat]
                            (attach-tag :polar [r t]))]
    (put :magnitude [:polar] magnitude)
    (put :angle [:polar] angle)
    (put :real-part [:polar] (typed/fn [x :- TaggedRawComplexInternal]
                               (mul (magnitude x)
                                    (cos (angle x)))))
    (put :imag-part [:polar] (typed/fn [x :- TaggedRawComplexInternal]
                               (mul (magnitude x)
                                    (sin (angle x)))))
    (put :make-from-mag-ang :polar make-from-mag-ang)
    (put :make-from-real-imag :polar (typed/fn [x :- TaggedFloat
                                                y :- TaggedFloat]
                                       (make-from-mag-ang (sqrt (add (square x)
                                                                     (square y)))
                                                          (atan2 y x)))))
  :done)
(install-polar-package)


(ann install-complex-package [-> (Val :done)])
(defn install-complex-package []
  (let [tag (typed/fn [x :- TaggedRawComplex] (attach-tag :complex x))
        make-from-real-imag (ignore-with-unchecked-cast
                             (get_ :make-from-real-imag :rectangular)
                             [TaggedFloat TaggedFloat -> TaggedRectangularComplex])
        make-from-mag-ang (ignore-with-unchecked-cast
                           (get_ :make-from-mag-ang :polar)
                           [TaggedFloat TaggedFloat -> TaggedPolarComplex])]
    (let [add-complex (typed/fn [x :- TaggedRawComplex y :- TaggedRawComplex]
                        (make-from-real-imag (add (real-part x) (real-part y))
                                             (add (imag-part x) (imag-part y))))
          sub-complex (typed/fn [x :- TaggedRawComplex y :- TaggedRawComplex]
                        (make-from-real-imag (sub (real-part x) (real-part y))
                                             (sub (imag-part x) (imag-part y))))
          mul-complex (typed/fn [x :- TaggedRawComplex y :- TaggedRawComplex]
                        (make-from-mag-ang (mul (magnitude x) (magnitude y))
                                           (add (angle x) (angle y))))
          div-complex (typed/fn [x :- TaggedRawComplex y :- TaggedRawComplex]
                        (make-from-mag-ang (div (magnitude x) (magnitude y))
                                           (sub (angle x) (angle y))))]
      (put :real-part [:complex] real-part)
      (put :imag-part [:complex] imag-part)
      (put :magnitude [:complex] magnitude)
      (put :angle [:complex] angle)
      (put :add [:complex :complex] (comp tag add-complex))
      (put :sub [:complex :complex] (comp tag sub-complex))
      (put :mul [:complex :complex] (comp tag mul-complex))
      (put :div [:complex :complex] (comp tag div-complex))
      (put :abs [:complex] magnitude)
      (put :equ? [:complex :complex] (typed/fn [x :- TaggedRawComplex y :- TaggedRawComplex]
                                       (and (equ? (real-part x)
                                                  (real-part y))
                                            (equ? (imag-part x)
                                                  (imag-part y)))))
      (put :=zero? [:complex] (typed/fn [x :- TaggedRawComplex]
                                (and (=zero? (real-part x))
                                     (=zero? (imag-part x)))))
      (put :negate [:complex] (typed/fn [x :- TaggedRawComplex]
                                ; `negate` here requires `[TaggedFloat -> TaggedFloat]`
                                (tag (make-from-real-imag (negate (real-part x))
                                                          (negate (imag-part x))))))
      (put :project [:complex] real-part)
      (put :make-from-real-imag :complex (typed/fn [x :- TaggedFloat y :- TaggedFloat]
                                           (tag (make-from-real-imag x y))))
      (put :make-from-mag-ang :complex (typed/fn [x :- TaggedFloat y :- TaggedFloat]
                                         (tag (make-from-mag-ang x y))))))
  :done)
(install-complex-package)
(ann ^:no-check make-complex-from-real-imag [TaggedFloat TaggedFloat -> TaggedComplex])
(def make-complex-from-real-imag (get_ :make-from-real-imag :complex))
(ann ^:no-check make-complex-from-mag-ang [TaggedFloat TaggedFloat -> TaggedComplex])
(def make-complex-from-mag-ang (get_ :make-from-mag-ang :complex))


;; (put-coercion :clojure-number :complex #(make-complex-from-real-imag (contents %) 0))


(ann numeric-tower [-> nil])
(deftest numeric-tower
  (is (equ? 1 1))
  (is (not (equ? 1 2)))
  (is (equ? (project (make-complex-from-real-imag (make-real 7) (make-real 5)))
            (make-real 7)))
  (is (equ? (add (div (make-complex-from-real-imag (make-real 1) (make-real 2))
                      (make-complex-from-real-imag (make-real 0.5) (make-real 1)))
                 (make-complex-from-real-imag (make-real 3) (make-real 4)))
            (make-complex-from-real-imag (make-real 5) (make-real 4))))
  (is (equ? (raise (raise (make-rational (make-integer 3) (make-integer 2))))
            (make-complex-from-real-imag (make-real 1.5) (make-real 0))))
  (is (equ? (drop_ (make-complex-from-real-imag (make-real 1) (make-real 0)))
            (make-integer 1))))


; 2.5.3 ----------------------------------------------------


(ann variable? (Pred Sym))
(def variable? symbol?)


(ann same-variable? (All [a b] [a b -> Boolean]))
(defn same-variable? [x y] (and (variable? x) (variable? y) (= x y)))


(ann coeff-term- [TermInternal -> TaggedNumber])
(def coeff-term- second)


(ann order-term- [TermInternal -> TaggedInteger])
(def order-term- first)


(declare make-term)
(ann install-term-package [-> (Val :done)])
(defn install-term-package []
   (let [tag (typed/fn [x :- TermInternal] (attach-tag :term x))]
     (put :coeff [:term] coeff-term-)
     (put :order [:term] order-term-)
     (put :negate [:term] (typed/fn [x :- TermInternal]
                            (make-term (order-term- x)
                                       (negate (coeff-term- x)))))
     (put :=zero? [:term] (comp =zero? coeff-term-))
     (put :make :term (typed/fn [o :- TaggedInteger c :- TaggedNumber]
                        (tag [o c]))))
   :done)
(install-term-package)


(ann ^:no-check make-term [TaggedInteger TaggedNumber -> Term])
(def make-term (get_ :make :term))


(ann ^:no-check order [Term -> TaggedInteger])
(defn order [t] (apply-generic :order t))


(ann ^:no-check coeff [Term -> TaggedNumber])
(defn coeff [t] (apply-generic :coeff t))


(ann ^:no-check empty-term-list? [TermList -> Boolean])
(defn empty-term-list? [l] (apply-generic :empty-term-list? l))


(ann ^:no-check first-term (IFn [NonEmptyTermList -> Term]
                                [EmptyTermList -> nil]))
(defn first-term [l] (apply-generic :first-term l))


(ann ^:no-check rest-terms (IFn [DenseTermList -> DenseTermList]
                                [SparseTermList -> SparseTermList]))
(defn rest-terms [l] (apply-generic :rest-terms l))


(ann ^:no-check adjoin-term [Term TermList -> TermList])
(defn adjoin-term [t l] (apply-generic :adjoin-term t l))


(ann the-empty-term-list '[SparseTermListTag '[]])
(def the-empty-term-list [:sparse-term-list []])


(ann my-drop-while (All [a] [[Any -> Boolean] (Seqable a) -> (Seqable a)]))
(defn my-drop-while [pred coll]
  (let [s (seq coll)]
    (if (and s (pred (first s)))
      (recur pred (rest s))
      coll)))


(ann install-dense-term-list-package [-> (Val :done)])
(defn install-dense-term-list-package []
  (let [tag (ann-form #(attach-tag :dense-term-list %)
                      (IFn [NonEmptyDenseTermListInternal -> NonEmptyDenseTermList]
                           [EmptyDenseTermListInternal -> EmptyDenseTermList]))
        empty-term-list?-impl empty?
        zero (make-integer 0)
        one (make-integer 1)]
    (put :empty-term-list? [:dense-term-list] empty-term-list?-impl)
    (put :first-term [:dense-term-list] (typed/fn [l :- NonEmptyDenseTermListInternal]
                                          (make-term (make-integer (dec (count l)))
                                                     (first l))))
    (put :rest-terms [:dense-term-list] (ignore-with-unchecked-cast
                                         #(tag (my-drop-while =zero? (rest %)))
                                         [DenseTermListInternal -> DenseTermList]))
    (put :negate [:dense-term-list] (ignore-with-unchecked-cast
                                     #(tag (map negate %))
                                     (IFn [EmptyDenseTermListInternal -> EmptyDenseTermList]
                                          [NonEmptyDenseTermListInternal -> NonEmptyDenseTermList])))
    (put :adjoin-term [:term :dense-term-list]
         (ignore-with-unchecked-cast
          (fn [t l]
            (tag (if (=zero? (coeff-term- t))
                   l
                   (cons (coeff-term- t)
                         (concat
                          (repeat
                           (contents
                            (sub (sub (order-term- t)
                                      (if (empty-term-list?-impl l)
                                          (negate one)
                                          (order (first-term (tag l)))))
                                 one))
                           zero)
                          l)))))
          (IFn [TermInternal DenseTermListInternal -> DenseTermList]))))
  :done)
(install-dense-term-list-package)


(ann install-sparse-term-list-package [-> (Val :done)])
(defn install-sparse-term-list-package []
  (let [empty-term-list?-impl empty?
        tag (ann-form
             (fn [l] (attach-tag :sparse-term-list l))
             (IFn [EmptySparseTermListInternal -> EmptySparseTermList]
                  [NonEmptySparseTermListInternal -> NonEmptySparseTermList]))]
    (put :empty-term-list? [:sparse-term-list] empty-term-list?-impl)
    (put :first-term [:sparse-term-list] (typed/fn [l :- NonEmptySparseTermListInternal]
                                           (let [[o c] (first l)]
                                             (make-term o c))))
    (put :rest-terms [:sparse-term-list] (ignore-with-unchecked-cast
                                          (comp tag rest)
                                          [SparseTermListInternal -> SparseTermList]))
    (put :negate [:sparse-term-list] (ignore-with-unchecked-cast
                                      (fn [l]
                                        (if (empty-term-list?-impl l)
                                          (tag l)
                                          (let [ts (tag l)]
                                            (adjoin-term (negate (first-term ts))
                                                         (negate (rest-terms ts))))))
                                      (IFn [EmptySparseTermListInternal -> EmptySparseTermList]
                                           [NonEmptySparseTermListInternal -> NonEmptySparseTermList])))
    (put :adjoin-term [:term :sparse-term-list] (ignore-with-unchecked-cast
                                                 (fn [t l]
                                                   (tag (if (=zero? (coeff-term- t))
                                                          l
                                                          (cons t l))))
                                                 [TermInternal SparseTermListInternal -> SparseTermList])))
  :done)
(install-sparse-term-list-package)


(typed/tc-ignore
(defn install-polynomial-package []
  (let [variable first
        term-list second]
    (letfn [(make-poly [v ts] [v ts])
            (add-terms [l1 l2]
              (cond
                (empty-term-list? l1) l2
                (empty-term-list? l2) l1
                :else (let [t1 (first-term l1)
                            t2 (first-term l2)]
                        (cond
                          (gt? (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms l1) l2))
                          (lt? (order t1) (order t2)) (adjoin-term t2 (add-terms l1 (rest-terms l2)))
                          :else (adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2)))
                                             (add-terms (rest-terms l1) (rest-terms l2)))))))
            (sub-terms [l1 l2] (add-terms l1 (negate l2)))
            (mul-term-by-all-terms [t1 l]
              (if (empty-term-list? l)
                the-empty-term-list
                (let [t2 (first-term l)]
                  (adjoin-term (make-term (add (order t1) (order t2))
                                          (mul (coeff t1) (coeff t2)))
                               (mul-term-by-all-terms t1 (rest-terms l))))))
            (mul-terms [l1 l2]
              (if (empty-term-list? l1)
                the-empty-term-list
                (add-terms (mul-term-by-all-terms (first-term l1) l2)
                           (mul-terms (rest-terms l1) l2))))
            (div-terms ; Q. 2.91
              [l1 l2]
              (if (empty-term-list? l1)
                [the-empty-term-list the-empty-term-list]
                (let [t1 (first-term l1)
                      t2 (first-term l2)]
                  (if (gt? (order t2) (order t1))
                    [the-empty-term-list l1]
                    (let [new-c (div (coeff t1) (coeff t2))
                          new-o (sub (order t1) (order t2))
                          quotient-t (make-term new-o new-c)
                          [quotient remainder] (div-terms (sub-terms l1 (mul-term-by-all-terms quotient-t l2)) l2)]
                      [(adjoin-term quotient-t quotient) remainder])))))
            (tag [p] (attach-tag :polynomial p))]
      (put :add [:polynomial :polynomial]
           (fn [a b]
             (tag (if (same-variable? (variable a) (variable b))
                    (make-poly (variable a)
                               (add-terms (term-list a)
                                          (term-list b)))
                    (throw (Exception.
                            (str "Polys not in same var " [a b])))))))
                                        ; Q. 2.88
      (put :sub [:polynomial :polynomial] #(add (tag %1) (negate (tag %2))))
      (put :mul [:polynomial :polynomial]
           (fn [a b]
             (tag
              (if (same-variable? (variable a) (variable b))
                (make-poly (variable a)
                           (mul-terms (term-list a)
                                      (term-list b)))
                (throw (Exception.
                        (str "Polys not in same var " [a b])))))))
      (put :div-poly [:polynomial :polynomial] (fn [a b]
                                                 (let [va (variable a)]
                                                   (if (same-variable? va (variable b))
                                                     (let [[quotient remainder] (div-terms (term-list a)
                                                                                           (term-list b))]
                                                       [(tag (make-poly va quotient))
                                                        (tag (make-poly va remainder))])
                                                      (throw (Exception.
                                                              (str "Polys not in save var " [a b])))))))
      (put :equ? [:polynomial :polynomial] #(=zero? (sub (tag %1) (tag %2))))
      (put :negate [:polynomial] (fn [p]
                                   (tag (let [v (variable p)
                                              ts (term-list p)]
                                          (make-poly v (negate ts))))))
                                        ; Q. 2.87
      (put :=zero? [:polynomial] #(loop [l (term-list %)]
                                    (or (empty-term-list? l)
                                        (and (=zero? (coeff (first-term l)))
                                             (recur (rest-terms l))))))
      (put :make :polynomial (fn [v ts] (tag (make-poly v ts))))))
  :done)
(install-polynomial-package)
(def make-polynomial (get_ :make :polynomial))
(defn div-poly [a b] (apply-generic :div-poly a b))


(deftest polynomial-test
  (is (=zero? (make-polynomial 'x the-empty-term-list)))
  (is (=zero? (sub [:polynomial
                    ['x [:sparse-term-list [[(make-integer 2) (make-real 1)]
                                            [(make-integer 1) (make-real 1)]]]]]
                   [:polynomial
                    ['x [:sparse-term-list [[(make-integer 2)
                                             (make-integer 1)]
                                            [(make-integer 1)
                                             (make-complex-from-real-imag
                                              (make-real 1)
                                              (make-real 0))]]]]])))
  (is (=zero? (sub [:polynomial ['x [:sparse-term-list [[(make-integer 2) (make-real 1)] [(make-integer 1) (make-real 1)]]]]]
                   [:polynomial ['x [:dense-term-list [(make-integer 1) (make-complex-from-real-imag (make-real 1) (make-real 0)) (make-integer 0)]]]])))
  (is (= (adjoin-term [:term [(make-integer 8) (make-integer 1)]] [:dense-term-list [(make-integer 2) (make-integer 1) (make-integer 0)]])
         [:dense-term-list [[:integer 1] [:integer 0] [:integer 0] [:integer 0] [:integer 0] [:integer 0] [:integer 2] [:integer 1] [:integer 0]]]))
  (is (=zero? (sub (add (make-polynomial 'x (adjoin-term (make-term (make-integer 2) (make-integer 3)) the-empty-term-list))
                        (make-polynomial 'x (adjoin-term (make-term (make-integer 1) (make-integer 3)) the-empty-term-list)))
                   [:polynomial ['x [:dense-term-list [(make-integer 3) (make-integer 3) (make-integer 0)]]]])))
  (is (equ? (mul (make-polynomial 'x (adjoin-term (make-term (make-integer 2) (make-integer 3)) the-empty-term-list))
                 (make-polynomial 'x (adjoin-term (make-term (make-integer 4) (make-integer 5)) the-empty-term-list)))
            [:polynomial ['x [:dense-term-list [(make-integer 15) (make-integer 0) (make-integer 0)
                                                (make-integer 0) (make-integer 0) (make-integer 0)
                                                (make-integer 0)]]]]))
  (let [[quotient remainder] (div-poly [:polynomial ['x [:dense-term-list [(make-integer 15) (make-integer 0) (make-integer 0)
                                                                           (make-integer 0) (make-integer 0) (make-integer 0)
                                                                           (make-integer 0)]]]]
                                       (make-polynomial 'x (adjoin-term (make-term (make-integer 2) (make-integer 3)) the-empty-term-list)))]
    (is (equ? quotient (make-polynomial 'x (adjoin-term (make-term (make-integer 4) (make-integer 5)) the-empty-term-list))))
    (is (=zero? remainder))))


;(clojure.test/run-tests *ns*)
); typed/tc-ignore
