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
  (if (number? datum)
    :clojure-number
    (first datum)))
(defn contents [datum]
  (if (number? datum)
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

(def operation-table (make-table))
(def get_ (operation-table :lookup))
(def put (operation-table :insert!))
(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)]
    (if-let [proc (get_ op type-tags)]
      (apply proc (map contents args))
(defn apply-generic [op & args]
      (throw (Exception. (str "no mtehod for these types: " op " " (apply list type-tags)))))))
  (let [type-tags (map type-tag args)
        error #(throw (Exception. (str "no mtehod for these types: " op args)))]
    (if-let [proc (get_ op type-tags)]
      (apply proc (map contents args))
      (if (apply = type-tags)
        (error)
        (loop [i 0
               t-high (type-tag (nth args i))
               ; hero @@@@@@@@@ なんとか実装して，とっとと終わらせる．
               ])))))


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
(defn square [x] (mul x x))
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

(defn raisable? [x]
  (get_ :raise [(type-tag x)]))

; clojure number package
(defn install-clojure-number-package []
  (let [tag #(attach-tag :clojure-number %)]
    (put :add [:clojure-number :clojure-number] #(tag (+ %1 %2)))
    (put :sub [:clojure-number :clojure-number] #(tag (- %1 %2)))
    (put :mul [:clojure-number :clojure-number] #(tag (* %1 %2)))
    (put :div [:clojure-number :clojure-number] #(tag (/ %1 %2)))
    (put :div-truncate [:clojure-number :clojure-number] #(tag (int (div %1 %2))))
    (put :abs [:clojure-number] #(tag (Math/abs %)))
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
(declare make-rational)
(defn install-integer-package []
  (let [tag #(attach-tag :integer %)]
    (put :add [:integer :integer] #(tag (add %1 %2)))
    (put :sub [:integer :integer] #(tag (sub %1 %2)))
    (put :mul [:integer :integer] #(tag (mul %1 %2)))
    (put :div [:integer :integer] #(make-rational %1 %2))
    (put :div-truncate [:integer :integer] #(tag (int (div %1 %2))))
    (put :abs [:integer] #(tag (abs %)))
    (put :lt? [:integer :integer] lt?)
    (put :gt? [:integer :integer] gt?)
    (put :rem_ [:integer :integer] #(tag (rem_ %1 %2)))
    (put :equ? [:integer :integer] equ?)
    (put :=zero? [:integer] =zero?)
    (put :raise [:integer] #(make-rational % 1))
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
                                                     (* (denom x)
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
    (put :abs [:rational] (fn [x] (tag (make-rat (abs (numer x))
                                                 (abs (denom x))))))
    (put :lt? [:rational :rational] #(lt? (raise (tag %1)) (raise (tag %2))))
    (put :gt? [:rational :rational] #(gt? (raise (tag %1)) (raise (tag %2))))
    (put :equ? [:rational :rational] (fn [x y] (and (equ? (numer x)
                                                          (numer y))
                                                    (equ? (denom x)
                                                          (denom y)))))
    (put =zero? [:rational] #(=zero? (div (numer %)
                                          (denom %))))
    (put :raise [:rational] #(div (->> % numer contents make-real)
                                  (->> % denom contents make-real)))
    (put :drop [:real] (fn [x] (let [denom-x (denom x)]
                                 (if (equ? (abs denom-x) 1)
                                   (mul (numer x) denom-x)
                                   (tag x)))))
    (put :make :rational #(tag (make-rat %1 %2))))
  :done)
(install-rational-package)
(def make-rational (get_ :make :rational))


; integer package
(declare make-complex-from-real-imag)
(defn install-real-package []
  (let [tag #(attach-tag :real %)]
    (put :add [:real :real] #(tag (add %1 %2)))
    (put :sub [:real :real] #(tag (sub %1 %2)))
    (put :mul [:real :real] #(tag (mul %1 %2)))
    (put :div [:real :real] #(tag (div %1 %2)))
    (put :abs [:real] #(tag (abs %)))
    (put :sin [:real] #(tag (sin %)))
    (put :cos [:real] #(tag (cos %)))
    (put :sqrt [:real] #(tag (sqrt %)))
    (put :atan2 [:real :real] #(tag (atan2 %1 %2)))
    (put :lt? [:real :real] lt?)
    (put :gt? [:real :real] gt?)
    (put :equ? [:real :real] equ?)
    (put :=zero? [:real] =zero?)
    (put :raise [:real] #(make-complex-from-real-imag % 0))
    (put :drop [:real] (fn [x] (let [rx (tag x)
                                     ix (make-integer x)]
                                 (if (equ? ix rx)
                                   ix
                                   rx))))
    (put :make :real tag))
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
      (put :real-part [:complex] #(make-real (real-part %)))
      (put :imag-part [:complex] #(make-real (imag-part %)))
      (put :magnitude [:complex] #(make-real (magnitude %)))
      (put :angle [:complex] #(make-real (angle %)))
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
      (put :drop [:complex] (fn [x] (let [rx (real-part x)]
                                      (if (equ? rx x)
                                        rx
                                        x))))
      (put :make-from-real-imag :complex #(tag (make-from-real-imag %1 %2)))
      (put :make-from-mag-ang :complex #(tag (make-from-mag-ang %1 %2))))))
(install-complex-package)
(def make-complex-from-real-imag (get_ :make-from-real-imag :complex))
(def make-complex-from-mag-ang (get_ :make-from-mag-ang :complex))


(put-coercion :clojure-number :complex #(make-complex-from-real-imag (contents %) 0))


(deftest numeric-tower
  (is (equ? 1 1))
  (is (not (equ? 1 2)))
  (is (equ? (add (div (make-complex-from-real-imag (make-real 1) (make-real 2))
                      (make-complex-from-real-imag (make-real 0.5) (make-real 1)))
                 (make-complex-from-real-imag (make-real 3) (make-real 4)))
            (make-complex-from-real-imag (make-real 5) (make-real 4))))
  (is (equ? (raise (raise (make-rational (make-integer 3) (make-integer 2))))
            (make-complex-from-real-imag 1.5 0))))


(clojure.test/run-tests *ns*)
