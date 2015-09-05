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


(ann ^:no-check caar (All [a] [(Pair (Pair a Any) Any) -> a]))
(def caar (comp car car))


(ann ^:no-check cadr (All [a] [(Pair Any (Pair a Any)) -> a]))
(def cadr (comp car cdr))


(ann ^:no-check cdar (All [a] [(Pair (Pair Any a) Any) -> a]))
(def cdar (comp cdr car))


(ann ^:no-check cddr (All [a] [(Pair Any (Pair Any a)) -> a]))
(def cddr (comp cdr cdr))


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


(ann ^:no-check
     my-list (All [a] (IFn [-> nil]
                           [a -> (Pair a nil)]
                           [a a * -> (List a)])))
(defn my-list [& xs]
  (let [impl (typed/fn imp [xs :- (Seqable a)]
               (when-let [s (seq xs)]
                 (my-cons (first s) (imp (rest s)))))]
    (impl xs)))


(ann pair? (Pred (Pair Any Any)))
(defn pair? [x]
  (instance? Pair x))


(ann any? (All [a] [[a -> Boolean] (U Any (List a)) -> Boolean]))
(defn any? [pred x]
  (and (pair? x)
       (or (pred (car x))
           (recur pred (cdr x)))))


(ann test-any? [-> nil])
(deftest test-any?
  (is (any? odd? (my-list 2 4 6 1 7)))
  (is (not (any? odd? (my-list 2 4 6))))
  )


(ann print-my-list [(List Any) -> nil])
(defn print-my-list [l]
  (let [head (car l)
        more (cdr l)]
    (print (str head " "))
    (if (pair? more)
      (recur more)
      (print "\n"))))
