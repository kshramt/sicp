(ns sicp.deque
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [All
            Any
            Atom1
            Atom2
            IFn
            Option
            Pred
            Seqable
            U
            Val
            Var1
            ann
            ]
    :as typed
    ]
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
   )
  (:import [sicp.pair Pair]))


(defmacro pef
  "print-env-form"
  [form]
  `(let [RETURN# ~form
         _# (typed/print-env ~(str form))]
     RETURN#))


(typed/defalias EmptyDeque Pair)
(typed/defalias NonEmptyDeque Pair)
(typed/defalias Deque (U EmptyDeque NonEmptyDeque))
(typed/defalias DequeNode Pair)


(ann make-deque [-> EmptyDeque])
(defn make-deque
  "Q 3.23"
  []
  (my-cons nil nil))


(ann make-deque-node [(Option DequeNode) Any (Option DequeNode) -> DequeNode])
(defn- make-deque-node [l x r]
  (my-cons x (my-cons l r)))


(ann val-deque-node [DequeNode -> Any])
(def val-deque-node car)


(ann ^:no-check ptrs-deque-node [DequeNode -> Pair])
(def ptrs-deque-node cdr)


(ann ^:no-check front-node-deque (IFn [EmptyDeque -> nil]
                                      [NonEmptyDeque -> DequeNode]))
(def front-node-deque car)


(ann ^:no-check rear-node-deque (IFn [EmptyDeque -> nil]
                                     [NonEmptyDeque -> DequeNode]))
(def rear-node-deque cdr)


(ann ^:no-check empty-deque? [Deque -> Boolean
                                :filters {:then (is EmptyDeque 0)
                                          :else (! EmptyDeque 0)}])
(def empty-deque? (comp nil? front-node-deque))


(ann front-deque [Deque -> Any])
(defn front-deque [q]
  (if (empty-deque? q)
    (throw (Exception. (str "front-deque called for empty deque")))
    (val-deque-node (front-node-deque q))))


(ann rear-deque [Deque -> Any])
(defn rear-deque [q]
  (if (empty-deque? q)
    (throw (Exception. (str "rear-deque called for empty deque")))
    (val-deque-node (rear-node-deque q))))


(ann front-insert-deque! [Deque Any -> NonEmptyDeque])
(defn front-insert-deque! [q x]
  (if (empty-deque? q)
    (let [n (make-deque-node nil x nil)]
      (set-car! q n)
      (set-cdr! q n))
    (let [front (front-node-deque q)
          n (make-deque-node nil x front)]
      (set-car! q n)
      (set-car! (ptrs-deque-node front) n)))
  q)


(ann rear-insert-deque! [Deque Any -> NonEmptyDeque])
(defn rear-insert-deque! [q x]
  (if (empty-deque? q)
    (let [n (make-deque-node nil x nil)]
      (set-car! q n)
      (set-cdr! q n))
    (let [rear (rear-node-deque q)
          n (make-deque-node rear x nil)]
      (set-cdr! q n)
      (set-cdr! (ptrs-deque-node rear) n)))
  q)


(ann front-delete-deque! [Deque -> Any])
(defn front-delete-deque! [q]
  (when (not (empty-deque? q))
    (let [front (front-node-deque q)
          rear (rear-node-deque q)]
      (if (= front rear)
        (do (set-car! q nil)
            (set-cdr! q nil))
        (let [new-front (cdr (ptrs-deque-node front DequeNode))]
          (set-car! q new-front)
          (set-car! (ptrs-deque-node new-front) nil))))))


(ann rear-delete-deque! [Deque -> Any])
(defn rear-delete-deque! [q]
  (when (not (empty-deque? q))
    (let [front (front-node-deque q)
          rear (rear-node-deque q)]
      (if (= front rear)
        (do (set-car! q nil)
            (set-cdr! q nil))
        (let [new-rear (car (ptrs-deque-node rear))]
          (set-cdr! q new-rear)
          (set-cdr! (ptrs-deque-node new-rear) nil))))))


(ann test-deque [-> nil])
(deftest test-deque []
  (let [q (make-deque)]
    (front-insert-deque! q 1)
    (is (front-deque q) 1)
    (front-insert-deque! q 2)
    (is (front-deque q) 2)
    (front-delete-deque! q)
    (is (front-deque q) 1)))
