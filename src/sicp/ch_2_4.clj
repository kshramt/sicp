(ns sicp.ch-2-4
  (:require [clojure.test :refer [is are deftest]]
            [clojure.pprint]
            [clojure.repl]))
(set! *warn-on-reflection* false)


(defmacro p- [x]
  `(let [x# ~x]
     (println ~(str &form))
     (clojure.pprint/pprint x#)
     x#))


(defn attach-tag [type-tag contents]
  (if (= type-tag :clojure-number)
    contents
    [type-tag contents]))
(defn type-tag [datum]
  (cond
   (number? datum) :clojure-number
   (instance? Boolean datum) :clojure-boolean
   :else (first datum)))
(defn contents [datum]
  (if (or (number? datum) (instance? Boolean datum))
    datum
    (second datum)))

(defn lookup
  ([key table]
     (if-let [[k v] (first table)]
       (if (= k key)
         v
         (recur key (rest table)))))
  ([key-1 key-2 table]
     (if-let [inner-table (lookup key-1 table)]
       (lookup key-2 inner-table))))

(defn insert
  ([key value table]
     (if-let [[k v :as kv] (first table)]
       (if (= k key)
         (cons [k value] (rest table))
         (cons kv (insert key value (rest table))))
       [[key value]]))
  ([key-1 key-2 value table]
     (if-let [inner-table (lookup key-1 table)]
        (insert key-1 (insert key-2 value inner-table) table)
        (insert key-1 (insert key-2 value []) table))))

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

(defn abs- [x] (if (pos? x) x (- x)))

(def operation-table (make-table))
(def get_ (operation-table :lookup))
(def put (operation-table :insert!))
(defn apply-generic-basic [op & args]
  (let [type-tags (map type-tag args)]
    (if-let [proc (get_ op type-tags)]
      (apply proc (map contents args))
      (throw (Exception. (str "no mtehod for these types: " op " " (apply list type-tags)))))))

(declare equ? raise)
(defn- projectable? [x]
  (get_ :project [(type-tag x)]))
(defn drop_ [x]
  (if-let [project- (projectable? x)]
    (let [xdown (project- (contents x))]
      (if (equ? (raise xdown) x)
        (recur xdown)
        x))
    x))

(defn- raisable? [x]
  (get_ :raise [(type-tag x)]))
(defn- raised-list [x]
  (if-let [raise- (raisable? x)]
    (cons x (raised-list (raise- (contents x))))
    [x]))
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
(defn- raise-up-to [x t]
  (if (= (type-tag x) t)
    x
    (if-let [raise- (raisable? x)]
      (recur (raise- (contents x)) t)
      (throw (Exception. (str "Unable to raise " x " up to " t))))))
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
(defn apply-generic-2-84
  "Q. 2.84"
  [op & args]
  (apply-generic-2-84- op args))
(defn apply-generic-2-86
  "Q. 2.86"
  [op & args]
  ((if (or (= op :raise)
           (= op :project)
           (= op :drop))
     identity
     drop_)
    (apply-generic-2-84- op args)))

(def apply-generic apply-generic-2-86)

(def coercion-table (make-table))
(def get-coercion (coercion-table :lookup))
(def put-coercion (coercion-table :insert!))

(defn real-part [x] (apply-generic :real-part x))
(defn imag-part [x] (apply-generic :imag-part x))
(defn magnitude [x] (apply-generic :magnitude x))
(defn angle [x] (apply-generic :angle x))
(defn add [x y] (apply-generic :add x y))
(defn sub [x y] (apply-generic :sub x y))
(defn mul [x y] (apply-generic :mul x y))
(defn negate [x] (apply-generic :negate x))
(defn div [x y] (apply-generic :div x y))
(defn div-truncate [x y] (apply-generic :div-truncate x y))
(defn sin [x] (apply-generic :sin x))
(defn cos [x] (apply-generic :cos x))
(defn abs [x] (apply-generic :abs x))
(defn sqrt [x] (apply-generic :sqrt x))
(defn atan2 [x y] (apply-generic :atan2 x y))
(defn lt? [x y] (apply-generic :lt? x y))
(defn gt? [x y] (apply-generic :gt? x y))
(defn rem_ [x y] (apply-generic :rem_ x y))
(defn equ? [x y] (apply-generic :equ? x y))
(defn =zero? [x] (apply-generic :=zero? x))
(defn raise [x] (apply-generic :raise x))
(defn project [x] (apply-generic :project x))
(defn square [x] (mul x x))
(defn le? [x y] (or (lt? x y) (equ? x y)))
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
(defn install-clojure-number-package []
  (let [tag #(attach-tag :clojure-number %)]
    (put :add [:clojure-number :clojure-number] #(tag (+ %1 %2)))
    (put :sub [:clojure-number :clojure-number] #(tag (- %1 %2)))
    (put :mul [:clojure-number :clojure-number] #(tag (* %1 %2)))
    (put :div [:clojure-number :clojure-number] #(tag (/ %1 %2)))
    (put :div-truncate [:clojure-number :clojure-number] #(tag (int (div %1 %2))))
    (put :negate [:clojure-number] #(tag (* -1 %)))
    (put :abs [:clojure-number] #(tag (abs- %)))
    (put :sin [:clojure-number] #(tag (Math/sin %)))
    (put :cos [:clojure-number] #(tag (Math/cos %)))
    (put :sqrt [:clojure-number] #(tag (Math/sqrt %)))
    (put :atan2 [:clojure-number :clojure-number] #(tag (Math/atan2 %1 %2)))
    (put :lt? [:clojure-number :clojure-number] <)
    (put :gt? [:clojure-number :clojure-number] >)
    (put :rem_ [:clojure-number :clojure-number] #(tag (rem %1 %2)))
    (put :equ? [:clojure-number :clojure-number] #(== %1 %2))
    (put :=zero? [:clojure-number] #(== % 0))
    (put :make :clojure-number tag))
  :done)
(install-clojure-number-package)
(def make-clojure-number (get_ :make :clojure-number))


; integer package
(declare make-rational make-integer make-real)
(defn install-integer-package []
  (let [tag #(attach-tag :integer %)]
    (put :add [:integer :integer] #(tag (add %1 %2)))
    (put :sub [:integer :integer] #(tag (sub %1 %2)))
    (put :mul [:integer :integer] #(tag (mul %1 %2)))
    (put :div [:integer :integer] #(make-rational (tag %1) (tag %2)))
    (put :div-truncate [:integer :integer] #(tag (int (div %1 %2))))
    (put :abs [:integer] #(tag (abs %)))
    (put :negate [:integer] #(tag (negate %)))
    (put :sqrt [:integer] #(sqrt (raise (tag %))))
    (put :cos [:integer] #(cos (raise (tag %))))
    (put :sin [:integer] #(sin (raise (tag %))))
    (put :lt? [:integer :integer] lt?)
    (put :gt? [:integer :integer] gt?)
    (put :rem_ [:integer :integer] #(tag (rem_ %1 %2)))
    (put :equ? [:integer :integer] equ?)
    (put :=zero? [:integer] =zero?)
    (put :raise [:integer] #(make-rational (tag %) (tag 1)))
    (put :make :integer #(tag (int %))))
  :done)
(install-integer-package)
(def make-integer (get_ :make :integer))
(def zero (make-integer 0))

; rational package
(declare make-real)
(defn install-rational-package []
  (let [tag #(attach-tag :rational %)
        numer first
        denom second
        make-rat (fn [n d]
                   (let [g (gcd n d)]
                     [(div-truncate n g) (div-truncate d g)]))]
    (put :add [:rational :rational] (fn [x y]
                                      (tag (make-rat (add (mul (numer x)
                                                               (denom y))
                                                          (mul (denom x)
                                                               (numer y)))
                                                     (mul (denom x)
                                                          (denom y))))))
    (put :sub [:rational :rational] (fn [x y]
                                      (tag (make-rat (sub (mul (numer x)
                                                               (denom y))
                                                          (mul (denom x)
                                                             (numer y)))
                                                     (mul (denom x)
                                                          (denom y))))))
    (put :mul [:rational :rational] (fn [x y]
                                      (tag (make-rat (mul (numer x)
                                                          (numer y))
                                                     (mul (denom x)
                                                          (denom y))))))
    (put :div [:rational :rational] (fn [x y]
                                      (tag (make-rat (mul (numer x)
                                                          (denom y))
                                                     (mul (denom x)
                                                          (numer y))))))
    (put :negate [:rational] #(tag (make-rat (negate (numer %))
                                             (denom %))))
    (put :abs [:rational] (fn [x] (tag (make-rat (abs (numer x))
                                                 (abs (denom x))))))
    (put :sqrt [:rational] #(sqrt (raise (tag %))))
    (put :cos [:rational] #(cos (raise (tag %))))
    (put :sin [:rational] #(sin (raise (tag %))))
    (put :lt? [:rational :rational] #(lt? (raise (tag %1)) (raise (tag %2))))
    (put :gt? [:rational :rational] #(gt? (raise (tag %1)) (raise (tag %2))))
    (put :equ? [:rational :rational] (fn [x y] (and (equ? (numer x)
                                                          (numer y))
                                                    (equ? (denom x)
                                                          (denom y)))))
    (put :=zero? [:rational] #(=zero? (div (numer %)
                                          (denom %))))
    (put :raise [:rational] #(make-real (div (contents (numer %))
                                             (contents (denom %)))))
    (put :project [:rational] #(div-truncate (numer %) (denom %)))
    (put :make :rational #(tag (make-rat %1 %2))))
  :done)
(install-rational-package)
(def make-rational (get_ :make :rational))

(defn- real->rational
  {:test #(do (is (= (real->rational 3.2) [16 5]))
              (is (= (real->rational 0.25) [1 4])))}
  [x]
  (letfn [(approx-equal [a b] (<= (abs- (- a b)) 1e-7))]
    (loop [a 1
           b 0
           c (bigint x)
           d 1
           y x]
      (if (approx-equal (/ c a) x)
        [c a]
        (let [y (/ 1 (- y (bigint y)))
              iy (bigint y)]
          (recur (+ (* a iy) b)
                 a
                 (+ (* c iy) d)
                 c
                 y))))))

; integer package
(declare make-complex-from-real-imag)
(defn install-real-package []
  (let [tag #(attach-tag :real %)]
    (put :add [:real :real] #(tag (add %1 %2)))
    (put :sub [:real :real] #(tag (sub %1 %2)))
    (put :mul [:real :real] #(tag (mul %1 %2)))
    (put :div [:real :real] #(tag (div %1 %2)))
    (put :abs [:real] #(tag (abs %)))
    (put :negate [:real] #(tag (negate %)))
    (put :sin [:real] #(tag (sin %)))
    (put :cos [:real] #(tag (cos %)))
    (put :sqrt [:real] #(tag (sqrt %)))
    (put :atan2 [:real :real] #(tag (atan2 %1 %2)))
    (put :lt? [:real :real] lt?)
    (put :gt? [:real :real] gt?)
    (put :equ? [:real :real] equ?)
    (put :=zero? [:real] =zero?)
    (put :raise [:real] #(make-complex-from-real-imag (tag %) (tag 0)))
    (put :project [:real] #(let [[n d] (real->rational %)]
                             (make-rational (make-integer n) (make-integer d))))
    (put :make :real #(tag (double %))))
  :done)
(install-real-package)
(def make-real (get_ :make :real))


; complex package
(defn install-rectangular-package []
  (let [real-part first
        imag-part second
        make-from-real-imag (fn [r i] (attach-tag :rectangular [r i]))]
    (put :real-part [:rectangular] real-part)
    (put :imag-part [:rectangular] imag-part)
    (put :magnitude [:rectangular] #(sqrt (add (square (real-part %))
                                               (square (imag-part %)))))
    (put :angle [:rectangular] #(atan2 (imag-part %)
                                       (real-part %)))
    (put :make-from-real-imag :rectangular make-from-real-imag)
    (put :make-from-mag-ang :rectangular (fn [x y]
                                           (make-from-real-imag (mul x (cos y))
                                                                (mul x (sin y))))))
  :done)
(install-rectangular-package)

(defn install-polar-package []
  (let [magnitude first
        angle second
        make-from-mag-ang (fn [r t] (attach-tag :polar [r t]))]
    (put :magnitude [:polar] magnitude)
    (put :angle [:polar] angle)
    (put :real-part [:polar] #(mul (magnitude %)
                                   (cos (angle %))))
    (put :imag-part [:polar] #(mul (magnitude %)
                                   (sin (angle %))))
    (put :make-from-mag-ang :polar make-from-mag-ang)
    (put :make-from-real-imag :polar #(make-from-mag-ang (sqrt (add (square %1)
                                                                    (square %2)))
                                                         (atan2 %2 %1))))
  :done)
(install-polar-package)

(defn install-complex-package []
  (let [tag #(attach-tag :complex %)
        make-from-real-imag (get_ :make-from-real-imag :rectangular)
        make-from-mag-ang (get_ :make-from-mag-ang :polar)]
    (letfn [(add-complex [x y]
              (make-from-real-imag (add (real-part x) (real-part y))
                                   (add (imag-part x) (imag-part y))))
            (sub-complex [x y]
              (make-from-real-imag (sub (real-part x) (real-part y))
                                   (sub (imag-part x) (imag-part y))))
            (mul-complex [x y]
              (make-from-mag-ang (mul (magnitude x) (magnitude y))
                                 (add (angle x) (angle y))))
            (div-complex [x y]
              (make-from-mag-ang (div (magnitude x) (magnitude y))
                                 (sub (angle x) (angle y))))]
      (put :real-part [:complex] #(make-real (contents (real-part %))))
      (put :imag-part [:complex] #(make-real (contents (imag-part %))))
      (put :magnitude [:complex] #(make-real (contents (magnitude %))))
      (put :angle [:complex] #(make-real (contents (angle %))))
      (put :add [:complex :complex] #(tag (add-complex %1 %2)))
      (put :sub [:complex :complex] #(tag (sub-complex %1 %2)))
      (put :mul [:complex :complex] #(tag (mul-complex %1 %2)))
      (put :div [:complex :complex] #(tag (div-complex %1 %2)))
      (put :abs [:complex] magnitude)
      (put :equ? [:complex :complex] (fn [x y] (and (equ? (real-part x)
                                                          (real-part y))
                                                    (equ? (imag-part x)
                                                          (imag-part y)))))
      (put :=zero? [:complex] #(and (=zero? (real-part %))
                                    (=zero? (imag-part %))))
      (put :negate [:complex] #(tag (make-from-real-imag (negate (real-part %)) (negate (imag-part %)))))
      (put :project [:complex] real-part)
      (put :make-from-real-imag :complex #(tag (make-from-real-imag %1 %2)))
      (put :make-from-mag-ang :complex #(tag (make-from-mag-ang %1 %2))))))
(install-complex-package)
(def make-complex-from-real-imag (get_ :make-from-real-imag :complex))
(def make-complex-from-mag-ang (get_ :make-from-mag-ang :complex))


(put-coercion :clojure-number :complex #(make-complex-from-real-imag (contents %) 0))



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


;(clojure.test/run-tests *ns*)

; 2.5.3 ----------------------------------------------------


(def variable? symbol?)
(defn same-variable? [x y] (and (variable? x) (variable? y) (= x y)))

(def coeff-term- second)
(def order-term- first)
(declare make-term)
(defn install-term-package []
   (letfn [(tag [t] (attach-tag :term t))]
     (put :coeff [:term] coeff-term-)
     (put :order [:term] order-term-)
     (put :negate [:term] #(make-term (order-term- %) (negate (coeff-term- %))))
     (put :=zero? [:term] #(=zero? (coeff-term- %)))
     (put :make :term (fn [o c] (tag [o c])))
     :done))
(install-term-package)
(def make-term (get_ :make :term))
(defn order [t] (apply-generic :order t))
(defn coeff [t] (apply-generic :coeff t))

(defn empty-term-list? [l] (apply-generic :empty-term-list? l))
(defn first-term [l] (apply-generic :first-term l))
(defn rest-terms [l] (apply-generic :rest-terms l))
(defn adjoin-term [t l] (apply-generic :adjoin-term t l))
(defn install-dense-term-list-package []
  (letfn [(tag [l] (attach-tag :dense-term-list l))]
    (put :empty-term-list? [:dense-term-list] empty?)
    (put :first-term [:dense-term-list] #(make-term (make-integer (dec (count %))) (first %))) ; todo: empty case
    (put :rest-terms [:dense-term-list] #(tag (rest %)))
    (put :negate [:dense-term-list] #(tag (map negate %)))
    (put :adjoin-term [:term :dense-term-list] (fn [t l]
                                                 (tag (if (=zero? (coeff-term- t))
                                                        l
                                                        (cons (coeff-term- t)
                                                              (concat (repeat (contents (sub (sub (order-term- t)
                                                                                                  (make-integer 1))
                                                                                             (order (first-term (tag l)))))
                                                                              (make-integer 0))
                                                                      l))))))
    :done))
(install-dense-term-list-package)

(defn install-sparse-term-list-package []
  (letfn [(tag [l] (attach-tag :sparse-term-list l))
          (negate- [ts] (if (empty-term-list? ts)
                          ts
                          (adjoin-term (negate (first-term ts))
                                       (negate- (rest-terms ts)))))]
    (put :empty-term-list? [:sparse-term-list] empty?)
    (put :first-term [:sparse-term-list] #(let [t (first %)]
                                            (make-term (first t)
                                                       (second t))))
    (put :rest-terms [:sparse-term-list] #(tag (rest %)))
    (put :negate [:sparse-term-list] #(negate- (tag %)))
    (put :adjoin-term [:term :sparse-term-list] (fn [t l]
                                                  (tag (if (=zero? (coeff-term- t))
                                                         l
                                                         (cons t l)))))
    :done))
(install-sparse-term-list-package)

(def the-empty-term-list [:sparse-term-list []])
(defn install-polynomial-package []
  (letfn [(make-poly [v ts] [v ts])
          (variable [p] (first p))
          (term-list [p] (second p))
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
          (add-poly [a b]
            (if (same-variable? (variable a) (variable b))
              (make-poly (variable a)
                         (add-terms (term-list a)
                                    (term-list b)))
              (throw (Exception. (str "Polys not in same var " [a b])))))
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
          (mul-poly [a b]
            (if (same-variable? (variable a) (variable b))
              (make-poly (variable a)
                         (mul-terms (term-list a)
                                    (term-list b)))
              (throw (Exception. (str "Polys not in same var " [a b])))))
          (tag [p] (attach-tag :polynomial p))]
    (put :add [:polynomial :polynomial] #(tag (add-poly %1 %2)))
    ; Q. 2.88
    (put :sub [:polynomial :polynomial] #(add (tag %1) (negate (tag %2))))
    (put :mul [:polynomial :polynomial] #(tag (mul-poly %1 %2)))
    (put :negate [:polynomial] (fn [p]
                                 (tag (let [v (variable p)
                                            ts (term-list p)]
                                        (make-poly v (negate ts))))))
    ; Q. 2.87
    (put :=zero? [:polynomial] #(loop [l (term-list %)]
                                  (or (empty-term-list? l)
                                      (and (=zero? (coeff (first-term l)))
                                           (recur (rest-terms l))))))
    (put :make :polynomial (fn [v ts] (tag (make-poly v ts)))))
  :done)
(install-polynomial-package)
(def make-polynomial (get_ :make :polynomial))


(deftest polynomial-test
  (is (=zero? (make-polynomial 'x the-empty-term-list)))
  (is (=zero? (sub [:polynomial ['x [:sparse-term-list [[(make-integer 2) (make-real 1)] [(make-integer 1) (make-real 1)]]]]]
                   [:polynomial ['x [:sparse-term-list [[(make-integer 2) (make-integer 1)] [(make-integer 1) (make-complex-from-real-imag (make-real 1) (make-real 0))]]]]])))
  (is (=zero? (sub [:polynomial ['x [:sparse-term-list [[(make-integer 2) (make-real 1)] [(make-integer 1) (make-real 1)]]]]]
                   [:polynomial ['x [:dense-term-list [(make-integer 1) (make-complex-from-real-imag (make-real 1) (make-real 0)) (make-integer 0)]]]])))
  (is (= (adjoin-term [:term [(make-integer 8) (make-integer 1)]] [:dense-term-list [(make-integer 2) (make-integer 1) (make-integer 0)]])
         [:dense-term-list [[:integer 1] [:integer 0] [:integer 0] [:integer 0] [:integer 0] [:integer 0] [:integer 2] [:integer 1] [:integer 0]]])))


