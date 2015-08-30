(ns sicp.table
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [Any
            Atom1
            Atom2
            IFn
            Kw
            Option
            Pred
            Seqable
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
            caar
            my-cons
            my-list
            pair?
            set-car!
            set-cdr!
            ]
    ])
  (:import [sicp.pair Pair]))


(ann assoc-table (IFn [Pair Any -> Any]
                      [Pair Any [Any Any -> Boolean] -> Any]))
(defn- assoc-table
  ([records key] (assoc-table records key =))
  ([records key same-key?]
   (cond (nil? records) false
         (same-key? key (caar records)) (car records)
         :else (recur (ignore-with-unchecked-cast (cdr records) Pair) key same-key?))))


(typed/defalias Table [-> [Kw -> [Any * -> Any]]])


(ann ^:no-check make-table-3-24 [-> Table])
(defn make-table-3-24
  "Q. 3.24"
  {:test #(let [t (make-table-3-24)]
            ((t :insert-proc) 1 2 3)
            (is ((t :lookup-proc) 1 2) 3))}
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


(ann ^:no-check make-table-3-25 [-> Table])
(defn make-table-3-25
  "Q. 3.25"
  {:test #(let [t (make-table-3-25)]
            ((t :insert-proc) [1 2] 3)
            (is ((t :lookup-proc) [1 2]) 3))}
  ([] (make-table-3-25 =))
  ([same-key?]
   (let [local-table (my-list :*table*)]
     (letfn [(lookup [ks]
               (when-let [record (assoc-table (cdr local-table) ks same-key?)]
                 (cdr record)))
             (insert! [ks v]
               (if-let [record (assoc-table (cdr local-table) ks same-key?)]
                   (set-cdr! record v)
                   (set-cdr! local-table
                             (my-cons (my-cons ks v)
                                      (cdr local-table))))
               :ok)
             (dispatch [m]
               (case m
                 :lookup-proc lookup
                 :insert-proc insert!
                 :else (throw (Exception. (str "Unknown operation --TABLE " m)))))]
       dispatch))))
