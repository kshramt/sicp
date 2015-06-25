(ns sicp.ch-3
  (:require [clojure.test :refer [is are deftest]]
            [clojure.pprint]
            [clojure.core.typed :refer [ann
                                        letfn>
                                        defalias
                                        ann-record
                                        ann-form
                                        Int Num
                                        Kw
                                        Any
                                        Atom1
                                        Option
                                        Pred
                                        Seqable
                                        Value
                                        Var1
                                        IFn
                                        All
                                        U
                                        ] :as typed]
            [clojure.core.typed.unsafe
             :refer
             [
              ignore-with-unchecked-cast
              ]]
            ))


(ann ^:no-check clojure.test/run-tests [-> '{:type Kw
                                             :test Int
                                             :pass Int
                                             :fail Int
                                             :error Int}])
(ann ^:no-check clojure.test/test-var [(Var1 [-> nil]) -> nil])


(ann make-account [Num -> [Kw -> [Num -> Num]]])
(defn make-account [balance]
  (let [local-balance (typed/atom :- Num balance)]
    (typed/fn [m :- Kw]
      (case m
        :withdraw (typed/fn [amount :- Num]
                    (let [b @local-balance]
                      (if (>= b amount)
                        (reset! local-balance (- b amount))
                        (throw (Exception. "Insufficient funds")))))
        :deposit (typed/fn [amount :- Num]
                   (reset! local-balance (+ @local-balance amount)))
        (throw (Exception. (str "Unknown request: " m)))))))


(ann make-accumulator [Num -> [Num -> Num]])
(defn make-accumulator
  "Q. 3.1"
  [x]
  (let [sum_ (typed/atom :- Num x)]
    (typed/fn [x :- Num]
      (reset! sum_ (+ @sum_ x)))))


(ann ^:no-check make-monitored
     (All [a b]
          [[a -> b] -> (IFn [(Value :how-many-calls?) -> Int]
                            [(Value :reset-count) -> (Value 0)]
                            [a -> b])]))
(defn make-monitored
  "Q. 3.2"
  [f]
  (let [counter (typed/atom :- Int 0)]
    (letfn> [fm :- (IFn [(Value :how-many-calls?) -> Int]
                        [(Value :reset-count) -> (Value 0)]
                        [a -> b])
             (fm [x]
                 (case x
                  :how-many-calls? @counter
                  :reset-count (reset! counter 0)
                  (do (reset! counter (inc @counter))
                      (f x))))]
      fm)))


(ann make-protected-account [Num String -> [Kw -> [Num String -> Num]]])
(defn make-protected-account
  "Q. 3.3"
  [balance password]
  (let [local-balance (typed/atom :- Num balance)]
    (typed/fn [m :- Kw]
      (case m
        :withdraw (typed/fn [amount :- Num
                             pass :- String]
                    (if (= pass password)
                      (let [b @local-balance]
                        (if (>= b amount)
                          (reset! local-balance (- b amount))
                          (throw (Exception. "Insufficient funds"))))
                      (throw (Exception. "Invalid password"))))
        :deposit (typed/fn [amount :- Num
                            pass :- String]
                   (if (= pass password)
                     (reset! local-balance (+ @local-balance amount))
                     (throw (Exception. "Invalid password"))))
        (throw (Exception. (str "Unknown request: " m)))))))


(ann make-protected-account-3-4 [Num String -> [Kw -> [Num String -> Num]]])
(defn make-protected-account-3-4
  "Q. 3.4"
  [balance password]
  (let [local-balance (typed/atom :- Num balance)
        n-invalid-password (typed/atom :- Int 0)]
    (typed/fn [m :- Kw]
      (case m
        :withdraw (typed/fn [amount :- Num
                             pass :- String]
                    (if (= pass password)
                      (let [b @local-balance]
                        (reset! n-invalid-password 0)
                        (if (>= b amount)
                          (reset! local-balance (- b amount))
                          (throw (Exception. "Insufficient funds"))))
                      (let [n @n-invalid-password]
                        (throw (if (>= n 6)
                                 (Exception. "Call the police")
                                 (do (reset! n-invalid-password (inc n))
                                     (Exception. "Invalid password")))))))
        :deposit (typed/fn [amount :- Num
                            pass :- String]
                   (if (= pass password)
                     (do (reset! n-invalid-password 0)
                         (reset! local-balance (+ @local-balance amount)))
                     (let [n @n-invalid-password]
                         (throw (if (>= n 6)
                                  (Exception. "Call the police")
                                  (do (reset! n-invalid-password (inc n))
                                      (Exception. "Invalid password")))))))
        (throw (Exception. (str "Unknown request: " m)))))))


(ann random-in-range [Num Num -> Num])
(defn random-in-range [low high]
  (+ low (rand (- high low))))


(ann monte-carlo [Int [-> Boolean] -> Num])
(defn monte-carlo [trials experiment]
  (typed/loop [trials-remaining :- Int trials
         trials-passed :- Int 0]
    (if (<= trials-remaining 0)
      (/ trials-passed trials)
      (if (experiment)
        (recur (dec trials-remaining) (inc trials-passed))
        (recur (dec trials-remaining) trials-passed)))))


(ann estimate-integral [[Num Num -> Boolean] Num Num Num Num Int -> Num])
(defn estimate-integral
  "Q. 3.5"
  {:test (fn [] (do (is (<= 2.15 (estimate-integral #(<= (Math/sqrt (+ (* %1 %1) (* %2 %2))) 1) -1 1 -1 1 1000) 4.15))))}
  [P x1 x2 y1 y2 n-iter]
  (* (- x2 x1)
     (- y2 y1)
     (monte-carlo n-iter
                  (typed/fn []
                    (P (random-in-range x1 x2)
                       (random-in-range y1 y2))))))


(ann my-rand (IFn [-> Double]
                  [Long -> Long]))
(let [r (typed/atom :- java.util.Random (java.util.Random.))]
  (defn my-rand
    "Q. 3.6"
  ([]
     (.nextDouble ^java.util.Random @r))
  ([seed]
     (reset! r (java.util.Random. seed))
     seed)))


(ann make-joint [[Kw -> [Num String -> Num]] String String ->
                 [Kw -> [Num String -> Num]]])
(defn make-joint
  "Q. 3.7"
  [acc pass pass-another]
  ((acc :deposit) 0 pass) ; check if `pass` is valid
  (typed/fn [m :- Kw]
    (typed/fn [amount :- Num
               password :- String]
      (if (= password pass-another)
        ((acc m) amount pass)
        ((acc m) amount (str pass "break pass"))))))


(ann f-3-8 [Num -> Num])
(let [y (typed/atom :- Num 1)]
  (defn f-3-8
    "Q. 3.8"
    [x]
    (reset! y (* @y x))))


; Q. 3.09: skip
; Q. 3.10: skip
; Q. 3.11: skip
; Q. 3.12: skip
; Q. 3.13: skip
; Q. 3.14: `reverse`
; Q. 3.15: skip

;; Q: 3.16
;; 3:
;; pa -> qa
;; pd -> ra

;; 4:
;; pa -> qa
;; qa -> ra
;; pd -> ra

;; 7:
;; pa -> qa
;; pd -> qa
;; qa -> ra
;; qd -> ra

;; ∞:
;; pd -> qa
;; qd -> ra
;; rd -> pa


(typed/defprotocol IPair
  (set-car! [p :- Pair x :- Any] :- Any)
  (set-cdr! [p :- Pair x :- Any] :- Any)
  (car [p :- Pair] :- Any)
  (cdr [p :- Pair] :- Any)
  (any? [p :- Pair pred :- [Any -> Boolean]] :- Boolean)
  )

(declare pair?)
(ann-record Pair [_car :- (Atom1 Any)
                  _cdr :- (Atom1 Any)])
(defrecord Pair [_car _cdr]
  IPair
  (set-car! [self x] (reset! _car x))
  (set-cdr! [self x] (reset! _cdr x))
  (car [self] @_car)
  (cdr [self] @_cdr)
  (any? [self pred]
    (or (pred (car self))
        (let [tail (cdr self)]
          (if (pair? tail)
            (any? tail pred)
            false))))
  )

(ann get-new-pair [-> Pair])
(defn get-new-pair [] (->Pair (typed/atom :- Any nil) (typed/atom :- Any nil)))

(ann my-cons [Any Any -> Pair])
(defn my-cons [a b] (let [new (get-new-pair)]
                      (set-car! new a)
                      (set-cdr! new b)
                      new))

(ann -my-list [(Option (Seqable Any)) -> (Option Pair)])
(defn -my-list [xs]
  (if-let [s (seq xs)]
    (my-cons (first s) (-my-list (rest s)))
    nil))

(ann my-list [Any * -> (Option Pair)])
(defn my-list [& xs]
  (-my-list xs))


(ann pair? (Pred Pair))
(defn pair? [x] (instance? Pair x))

(ann test-any? [-> nil])
(deftest test-any?
  (is (any? (my-list 2 4 6 1 7) odd?))
  (is (not (any? (my-list 2 4 6) odd?)))
  )


(ann count-pairs (IFn [Any -> Int]
                      [Any Pair -> Pair]))
(defn count-pairs
  "Q. 3.17"
  {:test #(do (is (= (count-pairs (my-cons 1 2)) 1))
              (is (= (count-pairs (my-cons (my-cons 1 2)
                                           (my-cons (my-cons 3 4)
                                                    5))) 4))
              (let [p1 (my-cons 1 2)
                    p2 (my-cons 1 p1)
                    p3 (my-cons p1 p2)
                    _ (set-car! p2 p2)]
                (is (= (count-pairs p3) 3))))}
  ([x] (car (count-pairs x (my-cons nil nil))))
  ([x counted]
   (if (not (pair? x))
     (my-cons 0 counted)
     (if (any? counted #(= % x))
       (my-cons 0 counted)
       (let [na-counted (count-pairs (car x) (my-cons x counted))
             nb-counted (count-pairs (cdr x) (ignore-with-unchecked-cast (cdr na-counted) Pair))]
         (my-cons (+ (ignore-with-unchecked-cast (car na-counted) Int)
                     (ignore-with-unchecked-cast (car nb-counted) Int)
                     1)
                  (cdr nb-counted)))))))


(ann has-loop?-3-18 (IFn [Pair -> Boolean]
                         [Pair Pair -> Boolean]))
(defn has-loop?-3-18
  "Q 3.18"
  {:test #(do (is (not (has-loop?-3-18 (my-list 1 2 3))))
              (is (has-loop?-3-18 (let [l (my-list 1 2 3)
                                        _ (set-cdr! (cdr (cdr l)) l)]
                                    l))))}
  ([p] (has-loop?-3-18 p (my-cons nil nil)))
  ([p seen]
   (or (any? seen #(= % p))
       (let [b (cdr p)]
         (and (pair? b)
              (recur b (my-cons p seen)))))))

(ann has-loop?-3-19 (IFn [Any -> Boolean]
                         [Any Pair -> Boolean]))
(defn has-loop?-3-19
  "Q 3.19"
  {:test #(do (is (not (has-loop?-3-19 (my-list 1 2 3))))
              (is (has-loop?-3-19 (let [l (my-list 1 2 3)
                                        _ (set-cdr! (cdr (cdr l)) l)]
                                    l))))}
  ([p]
   (and (pair? p)
        (has-loop?-3-19 (cdr p) p)))
  ([p seen]
   (and (pair? p)
        (or (= p seen)
            (let [p (cdr p)]
              (and (pair? p)
                   (or (= p seen)
                       (recur (cdr p) (cdr seen)))))))))

; Q 3.20: skip

(ann front-ptr [Pair -> Pair])
(defn front-ptr [queue]
  (car queue))

(ann rear-ptr [Pair -> Pair])
(defn rear-ptr [queue]
  (cdr queue))

(ann set-front-ptr! [Pair Any -> Any])
(defn set-front-ptr! [queue item]
  (set-car! queue item))

(ann set-rear-ptr! [Pair Any -> Any])
(defn set-rear-ptr! [queue item]
  (set-cdr! queue item))

(ann empty-queue? [Pair -> Boolean])
(defn empty-queue? [queue]
  (nil? (front-ptr queue)))

(ann make-queue [-> Pair])
(defn make-queue []
  (my-cons nil nil))

(ann front-queue [Pair -> Any])
(defn front-queue [queue]
  (if (empty-queue? queue)
    (throw (Exception. (str "FRONT called with an empty queue " queue)))
    (car (front-ptr queue))))

(ann insert-queue! [Pair Any -> Pair])
(defn insert-queue! [queue item]
  (let [new-pair (my-cons item nil)]
    (if (empty-queue? queue)
      (do (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
      (do (set-cdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))

(ann delete-queue! [Pair -> Pair])
(defn delete-queue! [queue]
  (if (empty-queue? queue)
    (throw (Exception. (str "DELETE-QUEUE! called with an empty queue " queue)))
    (do (set-front-ptr! queue (cdr (front-ptr queue)))
        queue)))

(ann print-my-list [Pair -> nil])
(defn print-my-list [l]
  (let [head (car l)
        more (cdr l)]
    (print (str head " "))
    (if (pair? more)
      (recur more)
      (print "\n"))))

(ann print-queue [Pair -> nil])
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
