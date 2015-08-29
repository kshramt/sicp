(ns sicp.pair
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [Any
            Atom1
            Atom2
            Option
            Pred
            Seqable
            Var1
            ann
            ]
    :as typed
    ]))


(ann ^:no-check clojure.test/test-var [(Var1 [-> nil]) -> nil])


(typed/defprotocol IPair
  (set-car! [p :- Pair x :- Any] :- Any)
  (set-cdr! [p :- Pair x :- Any] :- Any)
  (car [p :- Pair] :- Any)
  (cdr [p :- Pair] :- Any)
  )


(typed/ann-datatype
 Pair [_car :- (Atom1 Any)
       _cdr :- (Atom1 Any)])


(declare pair?)
(deftype Pair [_car _cdr]
 IPair
 (set-car! [self x] (reset! _car x))
 (set-cdr! [self x] (reset! _cdr x))
 (car [self] @_car)
 (cdr [self] @_cdr)
)


(ann ^:no-check caar [Pair -> Any])
(def caar (comp car car))


(ann ^:no-check cadr [Pair -> Any])
(def cadr (comp car cdr))


(ann ^:no-check cdar [Pair -> Any])
(def cdar (comp cdr car))


(ann ^:no-check cddr [Pair -> Any])
(def cddr (comp cdr cdr))


(typed/defn get-new-pair [] :- Pair
  (->Pair (typed/atom :- Any nil)
          (typed/atom :- Any nil)))


(typed/defn my-cons [a :- Any b :- Any] :- Pair
  (let [new (get-new-pair)]
    (set-car! new a)
    (set-cdr! new b)
    new))


(ann -my-list [(Option (Seqable Any)) -> (Option Pair)])
(defn- -my-list [xs]
  (when-let [s (seq xs)]
    (my-cons (first s) (-my-list (rest s)))))


(ann my-list [Any * -> (Option Pair)])
(defn my-list [& xs]
  (-my-list xs))


(ann pair? (Pred Pair))
(defn pair? [x]
  (instance? Pair x))


(ann any? [[Any -> Boolean] Any -> Boolean])
(defn any? [pred x]
  (and (pair? x)
       (or (pred (car x))
           (recur pred (cdr x)))))


(ann test-any? [-> nil])
(deftest test-any?
  (is (any? odd? (my-list 2 4 6 1 7)))
  (is (not (any? odd? (my-list 2 4 6))))
  )
