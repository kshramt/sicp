(ns sicp.ch-3
  (:require [clojure.test :refer [is are deftest]]
            [clojure.pprint]
            [clojure.core.typed :refer [ann
                                        letfn>
                                        Int Num
                                        Kw
                                        Value
                                        IFn
                                        All
                                        ] :as typed]
                                        ;:verbose
            )
  (:gen-class))


(ann ^:no-check clojure.test/run-tests [-> '{:type Kw
                                             :test Int
                                             :pass Int
                                             :fail Int
                                             :error Int}])


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


(typed/check-ns)
(clojure.test/run-tests)
