(ns sicp.table
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [Any
            Atom1
            Atom2
            IFn
            Option
            Pred
            Seqable
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
            caar
            my-cons
            my-list
            pair?
            set-car!
            set-cdr!
            ]
    ])
  (:import [sicp.pair Pair]))


(typed/tc-ignore ; typing dispatching fn is tedious
(defn- assoc-table
  ([records key] (assoc-table records key =))
  ([records key same-key?]
   (cond (nil? records) false
         (same-key? key (caar records)) (car records)
         :else (recur (cdr records) key same-key?))))


(defn make-table-3-24
  "Q. 3.24"
  ([] (make-table-3-24 =))
  ([same-key?]
   (let [local-table (my-list :*table*)]
     (letfn [(lookup [k1 k2]
               (when-let [subtable (assoc-table (cdr local-table) k1 same-key?)]
                 (when-let [record (assoc-table (cdr subtable) k2 same-key?)]
                   (cdr record))))
             (insert! [k1 k2 v]
               (if-let [subtable (assoc-table (cdr local-table) k1 same-key?)]
                 (if-let [record (assoc-table (cdr subtable) k2 same-key?)]
                   (set-cdr! record v)
                   (set-cdr! subtable
                             (my-cons (my-cons k2 v)
                                      (cdr subtable))))
                 (set-cdr! local-table
                           (my-cons (my-list k1 (my-cons k2 v))
                                    (cdr local-table))))
               :ok)
             (dispatch [m]
               (case m
                 :lookup-proc lookup
                 :insert-proc insert!
                 :else (throw (Exception. (str "Unknown operation --TABLE " m)))))]
       dispatch))))


(deftest test-make-table-3-24
  (let [t (make-table-3-24)]
    ((t :insert-proc) 1 2 3)
    (is ((t :lookup-proc) 1 2) 3)))
); typed/tc-ignore
