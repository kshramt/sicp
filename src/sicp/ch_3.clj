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
                                        Val
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
          [[a -> b] -> (IFn [(Val :how-many-calls?) -> Int]
                            [(Val :reset-count) -> (Val 0)]
                            [a -> b])]))
(defn make-monitored
  "Q. 3.2"
  [f]
  (let [counter (typed/atom :- Int 0)]
    (letfn> [fm :- (IFn [(Val :how-many-calls?) -> Int]
                        [(Val :reset-count) -> (Val 0)]
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
  ([x] (ignore-with-unchecked-cast
        (car (count-pairs x (my-cons nil nil)))
        Int))
  ([x counted]
   (if (not (pair? x))
     (my-cons 0 counted)
     (if (any? #(= % x) counted)
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
   (or (any? #(= % p) seen)
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
                       (recur (cdr p) (ignore-with-unchecked-cast
                                       (cdr seen)
                                       Pair)))))))))

; Q. 3.20: skip


; Q. 3.38
; a
; 50, 45, 40, 35

(typed/tc-ignore ; 3.38-b

(def balance-3-38 (atom 100))

(def peter-memory (atom 0))
(defn peter-out [] (reset! peter-memory @balance-3-38))
(defn peter-in [] (reset! balance-3-38 (+ @peter-memory 10)))

(def paul-memory (atom 0))
(defn paul-out [] (reset! paul-memory @balance-3-38))
(defn paul-in [] (reset! balance-3-38 (- @paul-memory 20)))

(def mary-memory-1 (atom 0))
(def mary-memory-2 (atom 0))
(defn mary-out-1 [] (reset! mary-memory-1 @balance-3-38))
(defn mary-out-2 [] (reset! mary-memory-2 @balance-3-38))
(defn mary-in-a [] (reset! balance-3-38 (- @mary-memory-1 (/ @mary-memory-2 2))))
(defn mary-in-b [] (reset! balance-3-38 (- @mary-memory-2 (/ @mary-memory-1 2))))

(declare all-orders-2)
(defn- all-orders-1 [xs ys zs]
  (when-let [xs (seq xs)]
    (let [x (first xs)]
      (map #(cons x %) (all-orders-2 (rest xs) ys zs)))))

(defn- all-orders-2 [xs ys zs]
  (if (or (seq xs) (seq ys) (seq zs))
    (concat (all-orders-1 xs ys zs)
            (all-orders-1 ys xs zs)
            (all-orders-1 zs xs ys))
    [[(fn [] @balance-3-38)]]))

(defn squash [fs]
  (if (seq (rest fs))
    (do ((first fs))
        (recur (rest fs)))
    ((first fs))))

(defn try-all-par []
  (map #(squash %)
       (map #(cons (fn [] (reset! balance-3-38 100)) %)
            (concat
             (all-orders-2 [peter-out peter-in]
                           [paul-out paul-in]
                           [mary-out-1 mary-out-2 mary-in-a])
             (all-orders-2 [peter-out peter-in]
                           [paul-out paul-in]
                           [mary-out-1 mary-out-2 mary-in-b])))))
(defn q-3-38-b
  "Q. 3.38-b
  25 30 35 40 45 50 55 60 65 70 80 90 110"
  []
  (sort (set (try-all-par))))

) ; typed/tc-ignore

; Q. 3.39 skip
; Q. 3.40 skip
; Q. 3.41 No
; Q. 3.42 Yes
; Q. 3.43 skip
; Q. 3.44 No
; Q. 3.45 Nesting the same serializer blocks execution.
; Q. 3.46 skip


(ann test-and-set! [(Atom1 Boolean) -> Boolean])
(defn test-and-set! [cell]
  (or @cell
      (do (reset! cell true)
          false)))

(ann clear! [(Atom1 Boolean) -> false])
(defn clear! [cell]
  (reset! cell false))


(ann make-mutex [-> [Kw -> Any]])
(defn make-mutex []
  (let [cell (typed/atom :- Boolean false)]
    (typed/fn [m :- Kw]
      (case m
        :acquire
        (when (test-and-set! cell)
          (recur :acquire))
        :release
        (clear! cell)))))


(ann make-serializer (All [a b] [-> [[a * -> b] -> [a * -> b]]]))
(defn make-serializer []
  (let [mutex (make-mutex)]
    (typed/fn [p :- [a * -> b]]
      (fn [& args]
        (mutex :acquire)
        (let [val (apply p args)]
          (mutex :release)
          val)))))


(ann dec-or-zero [Int -> Int])
(defn dec-or-zero [n]
  (ignore-with-unchecked-cast (max (dec n) 0) Int))


(ann make-semaphore-3-47-a [Int -> [Kw -> Any]])
(defn make-semaphore-3-47-a
  "Q. 3.47-a"
  [n]
  {:pre (pos? n)}
  (let [mutex (make-mutex)
        i (typed/atom :- Int 0)]
    (typed/fn [m :- Kw]
      (case m
        :acquire
        (do
          (mutex :acquire)
          (if (< @i n)
            (do (swap! i inc)
                (mutex :release))
            (do (mutex :release)
                ; theis :release is required to allow others to release the semaphore
                (recur m))))
        :release
        (do (mutex :acquire)
            (swap! i dec-or-zero)
            (mutex :release))))))


(ann make-semaphore-3-47-b [Int -> [Kw -> Any]])
(defn make-semaphore-3-47-b
  "Q. 3.47-b"
  [n]
  {:pre (pos? n)}
  (let [cell (typed/atom :- Boolean false)
        i (typed/atom :- Int 0)]
    (typed/fn [m :- Kw]
      (case m
        :acquire
        (if (test-and-set! cell)
          (recur m)
          (if (< @i n)
            (do (swap! i inc)
                (clear! cell))
            (do (clear! cell)
                (recur m))))
        :release
        (do (if (test-and-set! cell)
              (recur m)
              (do (swap! i dec-or-zero)
                  (clear! cell))))))))
