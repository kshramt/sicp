(ns sicp.constraint
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [All
            Any
            Atom1
            Atom2
            CountRange
            ExactCount
            I
            IFn
            Int
            Kw
            NonEmptySeqable
            Num
            Option
            Pred
            Seqable
            TFn
            U
            Val
            ann
            defalias
            letfn>
            ]
    :as typed
    ]
   [clojure.core.typed.unsafe
    :refer
    [
     ignore-with-unchecked-cast
     ]]
   [sicp.util
    :refer [
            p_
            pef
            sqrt
            ]]
   ))



(defalias Connector (TFn [[x :variance :covariant]] [Request -> x]))
(defalias EmptyConnector (TFn [[x :variance :covariant]]
                              (I (Connector x) (ExactCount 0))))
(defalias NonEmptyConnector (TFn [[x :variance :covariant]]
                              (I (Connector x) (CountRange 1))))
(defalias Request Kw)
(defalias Constraint [Request -> Any])
(defalias Informant Any)


(declare make-connector)
(defmacro t-make-connector
  "todo: is there beteter way?"
  [t]
  `(ignore-with-unchecked-cast (make-connector) (Connector ~t)))


(declare celsius-fahrenheit-converter)
(declare probe)
(declare set-value!)
(declare forget-value!)


(ann try1 [-> Any])
(defn try1 []
  (let [c (t-make-connector Num)
        f (t-make-connector Num)]
    (celsius-fahrenheit-converter c f)
    (probe "celsius temp" c)
    (probe "fahrenheit temp" f)
    (set-value! c 25 :user)
    ;(set-value! f 212 :user) ; error
    (forget-value! c :user)
    (set-value! f 212 :user)
    ))


(declare multiplier)
(declare adder)
(declare constant)


(ann celsius-fahrenheit-converter [(Connector Num) (Connector Num) -> (Val :ok)])
(defn celsius-fahrenheit-converter [c f]
  (let [u (t-make-connector Num)
        v (t-make-connector Num)
        w (t-make-connector Num)
        x (t-make-connector Num)
        y (t-make-connector Num)]
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y))
  :ok)


(ann ^:no-check has-value? (All [a]
                                [(Connector a) -> Boolean
                                 :filters {:then (is (NonEmptyConnector a) 0)
                                           :else (! (NonEmptyConnector a) 0)}]))
(defn has-value? [connector]
  (connector :has-value?))


(ann ^:no-check get-value (All [a]
                               [(Connector a) -> a]))
(defn get-value [connector]
  (connector :value))


(ann ^:no-check set-value! (All [a]
                                [(Connector a) a Informant -> Any]))
(defn set-value! [connector new-value informant]
  ((connector :set-value!) new-value informant))


(ann ^:no-check forget-value! (All [a]
                                   [(Connector a) Informant -> Any]))
(defn forget-value! [connector retractor]
  ((connector :forget) retractor))


(ann ^:no-check connect (All [a]
                             [(Connector a) Constraint -> Any]))
(defn connect [connector new-constraint]
  ((connector :connect) new-constraint))


(ann adder [(Connector Num) (Connector Num) (Connector Num) -> Constraint])
(defn adder [a1 a2 sum]
  (letfn> [process-new-value :- [-> Any]
           (process-new-value
            []
            (cond
              (and (has-value? a1) (has-value? a2))
              (set-value! sum
                          (+ (get-value a1) (get-value a2))
                          me)
              (and (has-value? a1) (has-value? sum))
              (set-value! a2
                          (- (get-value sum) (get-value a1))
                          me)
              (and (has-value? a2) (has-value? sum))
              (set-value! a1
                          (- (get-value sum) (get-value a2))
                          me)))
           process-forget-value :- [-> Any]
           (process-forget-value
            []
            (forget-value! sum me)
            (forget-value! a1 me)
            (forget-value! a2 me)
            (process-new-value))
           me :- Constraint
           (me
            [request]
            (case request
              :I-have-a-value (process-new-value)
              :I-lost-my-value (process-forget-value)
              (throw (Exception. (str "unknown request -- adder: " request)))))]
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me))


(ann inform-about-value [Constraint -> Any])
(defn inform-about-value [constraint]
  (constraint :I-have-a-value))


(ann inform-about-no-value [Constraint -> Any])
(defn inform-about-no-value [constraint]
  (constraint :I-lost-my-value))


(ann multiplier [(Connector Num) (Connector Num) (Connector Num) -> Constraint])
(defn multiplier [m1 m2 product]
  (letfn> [process-new-value :- [-> Any]
           (process-new-value
            []
            (cond
              (or (and (has-value? m1) (zero? (get-value m1)))
                  (and (has-value? m2) (zero? (get-value m2))))
              (set-value! product 0 me)
              (and (has-value? m1) (has-value? m2))
              (set-value! product
                          (* (get-value m1) (get-value m2))
                          me)
              (and (has-value? product) (has-value? m1))
              (set-value! m2
                          (/ (get-value product) (get-value m1))
                          me)
              (and (has-value? product) (has-value? m2))
              (set-value! m1
                          (/ (get-value product) (get-value m2))
                          me)))
           process-forget-value :- [-> Any]
           (process-forget-value
            []
            (forget-value! product me)
            (forget-value! m1 me)
            (forget-value! m2 me)
            (process-new-value))
           me :- Constraint
           (me
            [request]
            (case request
                :I-have-a-value (process-new-value)
                :I-lost-my-value (process-forget-value)
                (throw (Exception. (str "unknown request -- multiplier: " request)))))]
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me))


(ann constant (All [a] [a (Connector a) -> Constraint]))
(defn constant [value connector]
  (let [me (typed/fn [request :- Request]
             (throw (Exception. (str "unknown request -- constraint: " request))))]
    (connect connector me)
    (set-value! connector value me)
    me))


(ann probe (All [a] [Any (Connector a) -> Any]))
(defn probe [name connector]
  (let [print-probe (typed/fn [value :- Any]
                      (println (str "probe: " name " = " value)))
        process-new-value (typed/fn []
                            (print-probe (get-value connector)))
        process-forget-value (typed/fn []
                               (print-probe "?"))
        me (typed/fn [request :- Request]
             (case request
               :I-have-a-value (process-new-value)
               :I-lost-my-value (process-forget-value)
               (throw (Exception. (str "unknown request -- probe: " request)))))]
    (connect connector me)
    me))


(declare for-each-except)


(ann ^:no-check make-connector [-> (Connector nil)])
(defn make-connector
  []
  (let [value (typed/atom :- Any nil)
        informant (typed/atom :- Informant nil)
        constraints (typed/atom :- (Seqable Constraint) [])]
    (letfn> [set-my-value :- [Any Informant -> Any]
             (set-my-value
              [newval setter]
              (cond
                (not (has-value? me))
                (do (reset! value newval)
                    (reset! informant setter)
                    (for-each-except setter
                                     inform-about-value
                                     @constraints))
                (not (= @value newval))
                (throw (Exception. (str "contradiction: " (list @value newval))))
                :else :ignored))
             forget-my-value :- [Informant -> Any]
             (forget-my-value
              [retractor]
              (if (= retractor @informant)
                (do (reset! informant nil)
                    (for-each-except retractor
                                     inform-about-no-value
                                     @constraints))
                :ignored))
             connect :- [Constraint -> Any]
             (connect
              [new-constraint]
              (when (not (some (typed/fn [c :- Constraint] (= new-constraint c))
                               @constraints))
                (reset! constraints
                        (cons new-constraint @constraints)))
              (when (has-value? me)
                (inform-about-value new-constraint))
              :done)
             me :- Connector
             (me
              [request]
              (case request
                :has-value? (if @informant true false)
                :value @value
                :set-value! set-my-value
                :forget forget-my-value
                :connect connect
                :informant @informant
                (throw (Exception. (str "unknown operation -- connector: " request)))))]
      me)))


(ann for-each-except [Informant [Constraint -> Any] (Seqable Constraint) -> (Val :done)])
(defn for-each-except [exception procedure xs]
  (let [lop (typed/fn [items :- (Seqable Constraint)]
              (if-let [s (seq items)]
                (if (= (first s) exception)
                  (recur (rest s))
                  (do (procedure (first s))
                      (recur (rest s))))
                :done))]
    (lop xs)))


(ann averager [(Connector Num) (Connector Num) (Connector Num) -> Constraint])
(defn averager
  "Q. 3.33"
  {:test #(let [a (t-make-connector Num)
                b (t-make-connector Num)
                c (t-make-connector Num)]
            (averager a b c)
            (constant 1 a)
            (constant 3 b)
            (is (get-value c) 2))}
  [a b c]
  (let [two (t-make-connector Num)
        two-ave (t-make-connector Num)]
    (constant 2 two)
    (multiplier two c two-ave)
    (adder a b two-ave)))


; Q. 3.34: `a` and `b` in multiplier should be different objects


(ann squarer [(Connector Num) (Connector Num) -> Constraint])
(defn squarer
  "Q. 3.35"
  {:test #(let [a (t-make-connector Num)
                b (t-make-connector Num)]
            (squarer a b)
            (set-value! a 2 :user)
            (is (get-value b) 4)
            (forget-value! a :user)
            (set-value! b 9 :user)
            (is (get-value a) 3))}
  [a b]
  (letfn> [process-new-value :- [-> Any]
           (process-new-value
            []
            (if (has-value? b)
              (if (neg? (get-value b))
                (throw (Exception. (str "square less than 0 -- squarer: "
                                        (get-value b))))
                (set-value! a (sqrt (get-value b)) me))
              (set-value! b (let [va (get-value a)] (* va va)) me)))
           process-forget-value :- [-> Any]
           (process-forget-value
            []
            (forget-value! b me)
            (forget-value! a me))
           me :- Constraint
           (me
            [request]
            (case request
              :I-have-a-value (process-new-value)
              :I-lost-my-value (process-forget-value)
              (throw (Exception. (str "unknown request -- squarer: "
                                      request)))))]
    (connect a me)
    (connect b me)
    me))
