(ns sicp.scheme-util
  (:require
   [clojure.test :refer [is are deftest]]
   [sicp.pair
    :refer [
            car
            cdr
            my-cons
            my-list
            my-reverse
            ]
    ]
   [sicp.util
    :refer [
            p_
            pef
            ]]
   )
  )

(defn error
  ([] (error ""))
  ([msg] (error msg {}))
  ([msg map] (throw (ex-info msg map))))

(def null nil)
(def _nil (symbol "nil"))
(def _true (symbol "true"))
(def _false (symbol "false"))

(defn scheme-of
  {:test
   #(do
      (doseq [x [
                 '.
                 '(.)
                 '(1 .)
                 '(1 2 .)
                 '(. 1)
                 '(. 1 2)
                 '(1 . 2 3)
                 '(1 . 2 . 3)
                 ]]
        (is (thrown? clojure.lang.ExceptionInfo (scheme-of x))))
      (are [in ret] (= (scheme-of in) ret)
        '(1 . 2) (my-cons 1 2)
        '(1 2 3) (my-list 1 2 3)
        '(1 2 . 3) (my-cons 1 (my-cons 2 3))
        '(1 2 3 . 4) (my-cons 1 (my-cons 2 (my-cons 3 4)))
        '(define (f x . xs)
           (list nil () true false x))
        (my-list 'define (my-cons 'f (my-cons 'x 'xs))
                 (my-list 'list _nil nil _true _false 'x)))
      )
   }
  [exp]
  (letfn [(parse-seq [s]
            (if (seq s)
              (let [n-dot (count (filter #(= % '.) s))]
                (cond
                  (= n-dot 0) (apply my-list (map scheme-of s))
                  (= n-dot 1) (let [n (count s)]
                                (if (< n 3)
                                  (error (str "Invalid use of . (too few args): " exp))
                                  (if (= (nth s (- n 2)) '.)
                                    (loop [xs (seq (reverse (map scheme-of (take (- n 2) s))))
                                           ret (scheme-of (nth s (- n 1)))]
                                      (if xs
                                        (recur (next xs)
                                               (my-cons (first xs) ret))
                                        ret))
                                    (error (str "Invalid use of . (invalid locaition): " exp)))))
                  :else (error (str "Invalid use of . (multiple dots): " exp))))
              null))]
    (cond
      (= exp '.) (error (str "Invalid user of .: " exp))
      (nil? exp) _nil
      (true? exp) _true
      (false? exp) _false
      (sequential? exp) (parse-seq exp)
      :else exp)))

(defn list-cons
  {:test #(do
            (is (= (list-cons 1 2 3) (my-cons 1 (my-cons 2 3)))))}
  [x y & zs]
  (let [l (my-reverse (my-cons x (my-cons y (apply my-list zs))))]
    (loop [ret (car l)
           l (cdr l)]
      (if (nil? l)
        ret
        (recur (my-cons (car l) ret) (cdr l))))))
