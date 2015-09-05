(ns sicp.queue
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [All
            Any
            Atom1
            Atom2
            IFn
            Int
            Kw
            Option
            Pred
            Seqable
            TFn
            U
            Var1
            ann
            defalias
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
            cdr
            caar
            my-cons
            my-list
            pair?
            print-my-list
            set-car!
            set-cdr!
            ]
    ])
  (:import [sicp.pair Pair]))


(defalias EmptyQueue (Pair nil Any))
(defalias NonEmptyQueue (TFn [[a :variance :covariant]]
                             (Pair (List a) (Pair a nil))))
(defalias Queue (TFn [[a :variance :covariant]]
                     (U EmptyQueue
                        (NonEmptyQueue a))))
(defalias QueueNode (TFn [[a :variance :covariant]] (List a)))


(ann ^:no-check front-ptr (All [a]
                               (IFn [EmptyQueue -> nil]
                                    [(NonEmptyQueue a) -> (QueueNode a)])))
(def front-ptr car)


(ann ^:no-check rear-ptr (All [a]
                              (IFn [EmptyQueue -> nil]
                                   [(NonEmptyQueue a) -> (QueueNode a)])))
(def rear-ptr cdr)


(ann set-front-ptr! [(Queue Any) Any -> Any])
(def set-front-ptr! set-car!)


(ann set-rear-ptr! [(Queue Any) Any -> Any])
(def set-rear-ptr! set-cdr!)


(ann ^:no-check empty-queue? (All [a]
                                  [(Queue a) -> Boolean
                                   :filters {:then (is EmptyQueue 0)
                                             :else (! EmptyQueue 0)}]))
(def empty-queue? (comp nil? front-ptr))


(ann make-queue [-> EmptyQueue])
(defn make-queue []
  (my-cons nil nil))


(ann front-queue (All [a] [(Queue a) -> a]))
(defn front-queue [queue]
  (if (empty-queue? queue)
    (throw (Exception. (str "FRONT called with an empty queue " queue)))
    (car (front-ptr queue))))


(ann insert-queue! (All [a] [(Queue a) a -> Any]))
(defn insert-queue! [queue item]
  (let [new-pair (my-cons item nil)]
    (if (empty-queue? queue)
      (do (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair))
      (do (set-cdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)))))


(ann delete-queue! (All [a] [(Queue a) -> Any]))
(defn delete-queue! [queue]
  (if (empty-queue? queue)
    (throw (Exception. (str "DELETE-QUEUE! called with an empty queue " queue)))
    (set-front-ptr! queue (cdr (front-ptr queue)))))


(ann print-queue [(Queue Any) -> nil])
(defn print-queue
  "Q 3.21"
  [queue]
  (if (empty-queue? queue)
    (print "\n")
    (print-my-list (front-ptr queue))))


(ann test-queue [-> nil])
(deftest test-queue
  (is (let [q (make-queue)]
        (insert-queue! q 1)
        (insert-queue! q 2)
        (delete-queue! q)
        (print-queue q)
        q)))


(typed/tc-ignore
(defn make-queue-3-22
  "Q 3.22"
  []
  (let [_front-ptr (atom nil)
        _rear-ptr (atom nil)]
    (letfn [(front-ptr [] @_front-ptr)
            (rear-ptr [] @_rear-ptr)
            (set-front-ptr! [x] (reset! _front-ptr x))
            (set-rear-ptr! [x] (reset! _rear-ptr x))
            (empty-queue? [] (nil? (front-ptr)))
            (dispatch [m]
              (case m
                :front-ptr front-ptr
                :rear-ptr rear-ptr
                :set-front-ptr! set-front-ptr!
                :set-rear-ptr! set-rear-ptr!
                :empty-queue? empty-queue?
                :front-queue #(if (empty-queue?)
                                (throw (Exception. (str "FRONT called with an empty queue" @_front-ptr)))
                                (first (front-ptr)))
                :insert-queue! (fn [queue item]
                                 (let [new-pair (my-cons item nil)]
                                   (do (set-front-ptr! queue new-pair)
                                       (set-rear-ptr! queue new-pair)
                                       queue)
                                   (do (set-cdr! (rear-ptr queue) new-pair)
                                       (set-rear-ptr! queue new-pair)
                                       queue)))
                :delete-queue! (fn [queue]
                                 (if (empty-queue? queue)
                                   (throw (Exception. (str "DELETE-QUEUE! called with an empty queue " queue)))
                                   (do (set-front-ptr! queue (cdr (front-ptr queue)))
                                       queue)))))]
      dispatch)))
); typed/tc-ignore
