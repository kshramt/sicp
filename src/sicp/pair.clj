(ns sicp.pair
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [All
            Any
            Atom1
            Atom2
            EmptySeqable
            IFn
            Int
            NonEmptySeqable
            Option
            Pred
            Rec
            Seqable
            TFn
            U
            Var1
            ann
            ann-protocol
            defalias
            loop
            ]
    :as typed
    ]))


(ann ^:no-check clojure.test/test-var [(Var1 [-> nil]) -> nil])


(typed/defprotocol
    [[a :variance :covariant]
     [b :variance :covariant]]
  IPair
  (set-car! [p :- (IPair a b) x :- a] :- Any)
  (set-cdr! [p :- (IPair a b) x :- b] :- Any)
  (car [p :- (IPair a b)] :- a)
  (cdr [p :- (IPair a b)] :- b)
  )


(typed/ann-datatype
 [[a :variance :covariant]
  [b :variance :covariant]]
 Pair
 [_car :- (Atom1 a)
  _cdr :- (Atom1 b)]
 :unchecked-ancestors #{(IPair a b)})


(declare pair?)
(deftype Pair [_car _cdr]
 IPair
 (set-car! [self x] (reset! _car x))
 (set-cdr! [self x] (reset! _cdr x))
 (car [self] @_car)
 (cdr [self] @_cdr)
 java.util.Map$Entry
 (getKey [self] (car self))
 (getValue [self] (cdr self))
 Object
 (toString [self]
   (str "(" (car self) " . " (cdr self) ")"))
 (equals [self other]
   (and (pair? other)
        (= (car self) (car other))
        (= (cdr self) (cdr other))))
)


(ann caar (All [a] [(Pair (Pair a Any) Any) -> a]))
(defn caar [p] (-> p car car))


(ann cadddr (All [a] [(Pair Any (Pair Any (Pair Any (Pair a Any)))) -> a]))
(defn cadddr [p] (-> p cdr cdr cdr car))


(ann cadr (All [a] [(Pair Any (Pair a Any)) -> a]))
(defn cadr [p] (-> p cdr car))


(ann cdar (All [a] [(Pair (Pair Any a) Any) -> a]))
(defn cdar [p] (-> p car cdr))


(ann cddr (All [a] [(Pair Any (Pair Any a)) -> a]))
(defn cddr [p] (-> p cdr cdr))


(ann caadr (All [a] [(Pair Any (Pair (Pair a Any) Any)) -> a]))
(defn caadr [p] (-> p cdr car car))


(ann caddr (All [a] [(Pair Any (Pair Any (Pair a Any))) -> a]))
(defn caddr [p] (-> p cdr cdr car))


(ann cdadr (All [a] [(Pair Any (Pair (Pair Any a) Any)) -> a]))
(defn cdadr [p] (-> p cdr car cdr))


(ann cdddr (All [a] [(Pair Any (Pair Any (Pair Any a))) -> a]))
(defn cdddr [p] (-> p cdr cdr cdr))


(typed/defn get-new-pair [] :- (Pair nil nil)
  (->Pair (typed/atom :- nil nil)
          (typed/atom :- nil nil)))


(ann ^:no-check my-cons (All [a b] [a b -> (Pair a b)]))
(defn my-cons [a b]
  (let [new (get-new-pair)]
    (set-car! new a)
    (set-cdr! new b)
    new))


(defalias List (TFn [[a :variance :covariant]]
                    (Rec [this] (Pair a (Option this)))))


(declare my-reverse)
(ann ^:no-check my-list
     (All [a] (IFn [-> nil]
                   [a a * -> (List a)])))
(defn my-list
  {:test #(do
            (is (= (my-list)
                   nil))
            (is (= (my-list 1 2 3)
                   (my-cons 1 (my-cons 2 (my-cons 3 nil))))))}
  [& xs]
  (my-reverse
   (loop [xs :- (Option (Seqable a)) xs
          ret :- (Option (List a)) nil]
     (if-let [xs (seq xs)]
       (recur (rest xs)
              (my-cons (first xs) ret))
       ret))))


(ann pair? (Pred (Pair Any Any)))
(defn pair? [x]
  (instance? Pair x))


(ann any? (All [a] [[a -> Boolean] (U Any (List a)) -> Boolean]))
(defn any?
  {:test #(do
            (is (any? odd? (my-list 2 4 6 1 7)))
            (is (not (any? odd? (my-list 2 4 6)))))}
  [pred x]
  (and (pair? x)
       (or (pred (car x))
           (recur pred (cdr x)))))


(ann ^:no-check my-reverse
     (All [a] (IFn [nil -> nil]
                   [(List a) -> (List a)])))
(defn my-reverse
  {:test #(do
            (are [in out] (= in out)
              (my-reverse nil)
              nil
              (my-reverse (my-list 1))
              (my-list 1)
              (my-reverse (my-list 1 2))
              (my-list 2 1)))}
  [p]
  (letfn [(impl [in out]
            (if (nil? in)
              out
              (recur (cdr in)
                     (my-cons (car in)
                              out))))]
    (impl p nil)))


(ann ^:no-check
     my-map (All [a b c]
                 (IFn [[a -> b] nil -> nil]
                      [[a -> b] (List a) -> (List b)]
                      [[a b -> c] nil (List b) -> nil]
                      [[a b -> c] (List a) nil -> nil]
                      [[a b -> c] (List a) (List b) -> (List c)]
                      )))
(defn my-map
  {:test #(do
            (are [in out] (= in out)
              (my-map inc nil)
              nil
              (my-map inc (my-list 1 2))
              (my-list 2 3)
              (my-map + (my-list 1 2) (my-list 3 4 5))
              (my-list 4 6)))}
  ([f p]
   (letfn [(impl [p ret]
             (if (nil? p)
               ret
               (recur (cdr p)
                      (my-cons (f (car p))
                               ret))))]
     (my-reverse (impl p nil))))
  ([f p1 p2]
   (letfn [(impl [p1 p2 ret]
             (if (or (nil? p1)
                     (nil? p2))
               ret
               (recur (cdr p1)
                      (cdr p2)
                      (my-cons (f (car p1)
                                  (car p2))
                               ret))))]
     (my-reverse (impl p1 p2 nil)))))


(ann length (All [a] [(Option (List a)) -> Int]))
(defn length [p]
  (loop [p :- (Option (List a)) p
         n :- Int 0]
    (if (nil? p)
      n
      (recur (cdr p) (inc n)))))


(ann print-my-list [(List Any) -> nil])
(defn print-my-list [l]
  (let [head (car l)
        more (cdr l)]
    (print (str head " "))
    (if (pair? more)
      (recur more)
      (print "\n"))))
