(ns sicp.table
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
            List
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


(ann assoc-table (All [a b]
                      (IFn [(List (Pair a b)) Any -> Any]
                           [(List (Pair a b)) Any [Any Any -> Boolean] -> Any])))
(defn- assoc-table
  ([records key] (assoc-table records key =))
  ([records key same-key?]
   (cond (nil? records) false
         (same-key? key (caar records)) (car records)
         :else (recur (cdr records) key same-key?))))


(typed/defalias Table [Kw -> [Any * -> Any]])


(ann ^:no-check make-table-3-24 [-> Table])
(defn make-table-3-24
  "Q. 3.24"
  {:test #(let [t (make-table-3-24)]
            ((t :insert-proc) 1 2 3)
            (is (= ((t :lookup-proc) 1 2) 3)))}
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


(ann ^:no-check make-table-3-25 (IFn [-> Table]
                                     [[Any Any -> Boolean] -> Table]))
(defn make-table-3-25
  "Q. 3.25"
  {:test #(let [t (make-table-3-25)]
            ((t :insert-proc) [1 2] 3)
            (is (= ((t :lookup-proc) [1 2]) 3)))}
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


(typed/defalias Tree [Kw -> [Any * -> Any]])


(ann ^:no-check make-tree (IFn [-> Tree]
                                [[Any Any -> Int] -> Tree]))
(defn make-tree
  ([] (make-tree compare))
  ([compare-fn]
   (let [local-tree (my-cons :*tree* nil)
         kv-tree-node car
         k-tree-node (comp car kv-tree-node)
         v-tree-node (comp cdr kv-tree-node)
         lr-tree-node cdr]
     (letfn [(make-tree-node [k v]
               (my-cons (my-cons k v) (my-cons nil nil)))
             (set-k-tree-node! [n k]
               (set-car! (kv-tree-node n) k))
             (set-v-tree-node! [n v]
               (set-cdr! (kv-tree-node n) v))
             (insert-tree-node! [n k v]
               (case (compare-fn (k-tree-node n) k)
                 0
                 (set-v-tree-node! n v)
                 -1
                 (let [lr (lr-tree-node n)]
                   (if-let [l (car lr)]
                     (recur l k v)
                     (set-car! lr (make-tree-node k v))))
                  1
                  (let [lr (lr-tree-node n)]
                    (if-let [r (cdr lr)]
                      (recur r k v)
                      (set-cdr! lr (make-tree-node k v))))
                  (throw (Exception. (str "Invalid return value from compare-fn: "
                                          (compare-fn (k-tree-node n) k))))))
             (lookup-tree-node [n k]
               (case (compare-fn (k-tree-node n) k)
                 0
                 (v-tree-node n)
                 -1
                 (when-let [l (car (lr-tree-node n))]
                   (recur l k))
                 1
                 (when-let [r (cdr (lr-tree-node n))]
                   (recur r k))
                 (throw (Exception. (str "Invalid return value from compare-fn: "
                                         (compare-fn (k-tree-node n) k))))))
             (insert-tree [k v]
               (if-let [n (cdr local-tree)]
                 (insert-tree-node! n k v)
                 (set-cdr! local-tree (make-tree-node k v))))
             (lookup-tree [k]
               (when-let [n (cdr local-tree)]
                 (lookup-tree-node n k)))
             (dispatch [m]
               (case m
                 :insert insert-tree
                 :lookup lookup-tree
                 (throw (Exception. (str "Invalid m: " m)))))]
       dispatch))))


(ann ^:no-check make-table-3-26 (IFn [-> Table]
                                     [[Any Any -> Int] -> Table]))
(defn make-table-3-26
  "Q. 3.26"
  {:test #(let [t (make-table-3-26)]
            ((t :insert) [1 2] 3)
            (is (= ((t :lookup) [1 2]) 3))
            ((t :insert) [9 8] 7)
            (is (nil? ((t :lookup) [:not])))
            (is (= ((t :lookup) [9 8]) 7)))}
  ([] (make-table-3-26 compare))
  ([compare-fn]
   (make-tree compare-fn)))


(ann ^:no-check my-memoize (All [a b] [[a -> b] -> [a -> b]]))
(defn my-memoize
  [f]
  (let [table (make-table-3-26)]
    (letfn [(lookup [x] ((table :lookup) x))
            (insert! [x y] ((table :insert) x y))]
      (fn [x]
        (if-let [y-pre (lookup x)]
          y-pre
          (let [y (f x)]
            (insert! x y)
            y))))))


(ann fib [Int -> Int])
(defn fib [n]
  (case n
    0 0
    1 1
    (+ (fib (- n 1))
       (fib (- n 2)))))


(ann memo-fib [Int -> Int])
(def memo-fib (my-memoize
               (typed/fn [n :- Int] :- Int
                 (case n
                   0 0
                   1 1
                   (+ (memo-fib (- n 1))
                      (memo-fib (- n 2)))))))

(ann test-memo-fib [-> nil])
(deftest test-memo-fib
  (is (= (memo-fib 37)
         (fib 37))))


; Q. 3.27 skip
