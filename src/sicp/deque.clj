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
            Rec
            Seqable
            TFn
            U
            Val
            Var1
            ann
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
            ]]
   )
  (:import [sicp.pair Pair]))


(typed/defalias EmptyDeque (Pair nil Any))
(typed/defalias DequeNode (TFn [[a :variance :covariant]]
                               (Rec [this]
                                    (Pair a
                                          (Pair (Option this)
                                                (Option this))))))
(typed/defalias NonEmptyDeque (TFn [[a :variance :covariant]] (Pair (DequeNode a)
                                                                    (DequeNode a))))
(typed/defalias Deque (TFn [[a :variance :covariant]]
                           (U EmptyDeque (NonEmptyDeque a))))


(ann make-deque [-> EmptyDeque])
(defn make-deque
  "Q 3.23"
  []
  (my-cons nil nil))


(ann make-deque-node (All [a] [(Option (DequeNode a)) a (Option (DequeNode a)) -> (DequeNode a)]))
(defn- make-deque-node [l x r]
  (my-cons x (my-cons l r)))


(ann val-deque-node (All [a] [(DequeNode a) -> a]))
(def val-deque-node car)


(ann ^:no-check ptrs-deque-node (All [a] [(DequeNode a) -> (Pair (Option (DequeNode a))
                                                                 (Option (DequeNode a)))]))
(def ptrs-deque-node cdr)


(ann ^:no-check front-node-deque (All [a] (IFn [EmptyDeque -> nil]
                                               [(NonEmptyDeque a) -> (DequeNode a)])))
(def front-node-deque car)


(ann ^:no-check rear-node-deque (All [a] (IFn [EmptyDeque -> nil]
                                              [(NonEmptyDeque a) -> (DequeNode a)])))
(def rear-node-deque cdr)


(ann ^:no-check empty-deque? (All [a] [(Deque a) -> Boolean
                                       :filters {:then (is EmptyDeque 0)
                                                 :else (! EmptyDeque 0)}]))
(def empty-deque? (comp nil? front-node-deque))


(ann front-deque (All [a] [(NonEmptyDeque a) -> a]))
(defn front-deque [q]
  (if (empty-deque? q)
    (throw (Exception. (str "front-deque called for empty deque")))
    (val-deque-node (front-node-deque q))))


(ann rear-deque (All [a] [(NonEmptyDeque a) -> a]))
(defn rear-deque [q]
  (if (empty-deque? q)
    (throw (Exception. (str "rear-deque called for empty deque")))
    (val-deque-node (rear-node-deque q))))


(ann front-insert-deque! (All [a] [(Deque a) a -> Any]))
(defn front-insert-deque! [q x]
  (if (empty-deque? q)
    (let [n (make-deque-node nil x nil)]
      (set-car! q n)
      (set-cdr! q n))
    (let [front (front-node-deque q)
          n (make-deque-node nil x front)]
      (set-car! q n)
      (set-car! (ptrs-deque-node front) n))))


(ann rear-insert-deque! (All [a] [(Deque a) a -> Any]))
(defn rear-insert-deque! [q x]
  (if (empty-deque? q)
    (let [n (make-deque-node nil x nil)]
      (set-car! q n)
      (set-cdr! q n))
    (let [rear (rear-node-deque q)
          n (make-deque-node rear x nil)]
      (set-cdr! q n)
      (set-cdr! (ptrs-deque-node rear) n))))


(ann front-delete-deque! (All [a] [(Deque a) -> Any]))
(defn front-delete-deque! [q]
  (when (not (empty-deque? q))
    (let [front (front-node-deque q)
          rear (rear-node-deque q)]
      (if (= front rear)
        (do (set-car! q nil)
            (set-cdr! q nil))
        (let [new-front (ignore-with-unchecked-cast
                         (cdr (ptrs-deque-node front))
                         (DequeNode a))]
          (set-car! q new-front)
          (set-car! (ptrs-deque-node new-front) nil))))))


(ann rear-delete-deque! (All [a] [(Deque a) -> Any]))
(defn rear-delete-deque! [q]
  (when (not (empty-deque? q))
    (let [front (front-node-deque q)
          rear (rear-node-deque q)]
      (if (= front rear)
        (do (set-car! q nil)
            (set-cdr! q nil))
        (let [new-rear (ignore-with-unchecked-cast
                        (car (ptrs-deque-node rear))
                        (DequeNode a))]
          (set-cdr! q new-rear)
          (set-cdr! (ptrs-deque-node new-rear) nil))))))


(ann test-deque [-> nil])
(deftest test-deque []
  (let [q (make-deque)]
    (front-insert-deque! q 1)
    (is (= (front-deque q) 1))
    (front-insert-deque! q 2)
    (is (= (front-deque q) 2))
    (front-delete-deque! q)
    (is (= (front-deque q) 1))))
