(ns sicp.digital-circuit-simulator
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
            ASeq
            NonEmptySeqable
            Option
            Pred
            Seqable
            U
            Val
            ann
            defalias
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
    ]
   [sicp.queue
    :refer [
            Queue
            delete-queue!
            empty-queue?
            front-queue
            insert-queue!
            make-queue
            ]
    ]
   [sicp.util
    :refer [
            p_
            pef
            ]]
   )
  (:import [sicp.pair Pair]))


(defalias Signal Int)
(defalias Action [-> Any])
(defalias Wire (IFn [(Val :get-signal) -> Signal]
                    [(Val :set-signal!) -> [Signal -> (Option (Val :done))]]
                    [(Val :add-action!) -> [Action -> Any]]))
(defalias Time Int)
(defalias ActionQueue (Queue Action))
(defalias Segment (Pair Time ActionQueue))
(defalias Segments (List Segment))
(defalias EmptyAgenda (Pair Time nil))
(defalias NonEmptyAgenda (Pair Time Segments))
(defalias Agenda (U EmptyAgenda NonEmptyAgenda))


(ann make-segment [Time ActionQueue -> Segment])
(def make-segment my-cons)


(ann ^:no-check segment-time [Segment -> Time])
(def segment-time car)


(ann ^:no-check segment-queue [Segment -> ActionQueue])
(def segment-queue cdr)


(ann ^:no-check make-agenda [-> EmptyAgenda])
(defn make-agenda []
  (my-list 0))


(ann ^:no-check current-time [Agenda -> Time])
(def current-time car)


(ann set-current-time! [Agenda Time -> Any])
(def set-current-time! set-car!)


(ann ^:no-check segments (IFn [EmptyAgenda -> nil]
                              [NonEmptyAgenda -> Segments]
                              [Agenda -> (Option Segments)]))
(def segments cdr)


(ann ^:no-check set-segments! [Agenda (Option Segments) -> Any])
(def set-segments! set-cdr!)


(ann ^:no-check first-of-segments [Segments -> Segment])
(def first-of-segments car)


(ann first-segment [NonEmptyAgenda -> Segment])
(def first-segment (comp first-of-segments segments))


(ann ^:no-check rest-of-segments [Segments -> (Option Segments)])
(def rest-of-segments cdr)


(ann rest-segments [NonEmptyAgenda -> (Option Segments)])
(def rest-segments (comp rest-of-segments segments))


(ann ^:no-check empty-agenda? [Agenda -> Boolean
                               :filters {:then (is EmptyAgenda 0)
                                         :else (! EmptyAgenda 0)}])
(def empty-agenda? (comp nil? segments))


(ann add-to-agenda! [Time Action Agenda -> Any])
(defn add-to-agenda! [time action agenda]
  (let [belongs-before? (typed/fn [segments :- (Option Segments)]
                          (or (nil? segments)
                              (< time (segment-time (first-of-segments segments)))))
        make-new-segment (typed/fn [time :- Time
                                    action :- Action]
                           (let [q (make-queue)]
                             (insert-queue! q action)
                             (make-segment time q)))
        add-to-segment! (typed/fn [segments :- Segments]
                          (if (= (segment-time (first-of-segments segments)) time)
                            (insert-queue! (segment-queue (first-of-segments segments))
                                           action)
                            (let [more (rest-of-segments segments)]
                              (if (belongs-before? more)
                                (set-cdr! segments
                                          (my-cons (make-new-segment time action)
                                                   (cdr segments)))
                                (recur more)))))]
    (let [segments (segments agenda)]
      (if (belongs-before? segments)
        (set-segments!
         agenda
         (my-cons (make-new-segment time action)
               segments))
        (add-to-segment! segments)))))


(ann remove-first-agenda-item! [NonEmptyAgenda -> Any])
(defn remove-first-agenda-item! [agenda]
  (let [q (segment-queue (first-segment agenda))]
    (delete-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))


(ann first-agenda-item (IFn [EmptyAgenda -> nil]
                            [NonEmptyAgenda -> Action]))
(defn first-agenda-item [agenda]
  (if (empty-agenda? agenda)
    (throw (Exception. (str "Agenda is empty -- first-agenda-item")))
    (let [first-seg (first-segment agenda)]
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))


(ann the-agenda Agenda)
(def the-agenda (make-agenda))


(ann inverter-delay Int)
(def inverter-delay 1)


(ann and-gate-delay Int)
(def and-gate-delay 10000)


(ann or-gate-delay Int)
(def or-gate-delay 100000000)


(ann after-delay [Int Action -> Any])
(defn after-delay [delay action]
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))


(ann propagate [-> (Val :done)])
(defn propagate []
  (let [impl (typed/fn [a :- Agenda]
               (if (empty-agenda? a)
                 :done
                 (do
                   ((first-agenda-item a))
                   (remove-first-agenda-item! a)
                   (recur a))))]
    (impl the-agenda)))


(ann call-each [(Seqable [-> Any]) -> (Val :done)])
(defn call-each [procedures]
  (if-let [s (seq procedures)]
    (do ((first s))
        (recur (rest s)))
    :done))


(ann ^:no-check make-wire [-> Wire])
(defn make-wire []
  (let [signal-value (typed/atom :- Signal 0)
        action-procedure (typed/atom :- (Seqable Action) [])]
    (let [set-my-signal! (typed/fn [new-value :- Signal]
                           (when (not= @signal-value new-value)
                             (do (reset! signal-value new-value)
                                 (call-each @action-procedure))))
          accept-action-procedure! (typed/fn [proc :- Action]
                                     (reset! action-procedure (cons proc @action-procedure))
                                     (proc)) ; Q. 3.31 invoke the new connection
          dispatch (typed/fn [m :- Kw]
                     (case m
                       :get-signal @signal-value
                       :set-signal! set-my-signal!
                       :add-action! accept-action-procedure!
                       (throw (Exception. (str "Unknown operation -- WIRE " m)))))]
      dispatch)))


(ann get-signal [Wire -> Signal])
(defn get-signal [wire]
  (wire :get-signal))


(ann set-signal! [Wire Signal -> (Option (Val :done))])
(defn set-signal! [wire new-value]
  ((wire :set-signal!) new-value))


(ann add-action! [Wire Action -> Any])
(defn add-action! [wire action-procedure]
  ((wire :add-action!) action-procedure))


(defmacro invalid-signal-error [s]
  `(throw (Exception. (str "Invalid signal " ~s))))


(ann logical-not [Signal -> Signal])
(defn logical-not [^long s]
  (case s
    0 1
    1 0
    (invalid-signal-error s)))


(ann inverter [Wire Wire -> (Val :ok)])
(defn inverter [input output]
  (let [invert-input (typed/fn []
                       (after-delay
                        inverter-delay
                        #(set-signal!
                          output
                          (logical-not (get-signal input)))))]
    (add-action! input invert-input))
  :ok)


(ann logical-and [Signal Signal -> Signal])
(defn logical-and [^long s1 ^long s2]
  (case s1
    0 0
    1 (case s2
        0 0
        1 1
        (invalid-signal-error s2))
    (invalid-signal-error s1)))


(ann and-gate [Wire Wire Wire -> (Val :ok)])
(defn and-gate [a1 a2 output]
  (let [and-action-procedure (typed/fn []
                               (after-delay
                                and-gate-delay
                                (typed/fn []
                                  (set-signal!
                                   output
                                   (logical-and (get-signal a1)
                                                (get-signal a2))))))]
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure))
  :ok)


(ann logical-or [Signal Signal -> Signal])
(defn logical-or [^long s1 ^long s2]
  (case s1
    0 (case s2
        0 0
        1 1
        (invalid-signal-error s2))
    1 1
    (invalid-signal-error s1)))


(ann or-gate-3-28 [Wire Wire Wire -> (Val :ok)])
(defn or-gate-3-28
  "Q. 3.28"
  [a1 a2 output]
  (let [or-action-procedure (typed/fn []
                              (after-delay
                               or-gate-delay
                               (typed/fn []
                                 (set-signal!
                                  output
                                  (logical-or (get-signal a1)
                                              (get-signal a2))))))]
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure))
  :ok)


(ann or-gate-3-29 [Wire Wire Wire -> (Val :ok)])
(defn or-gate-3-29
  "Q. 3.29
  2*inverter-delay + and-gate-delay"
  [a1 a2 output]
  (let [n1 (make-wire)
        n2 (make-wire)
        n (make-wire)]
    (inverter a1 n1)
    (inverter a2 n2)
    (and-gate n1 n2 n)
    (inverter n output))
  :ok)


(ann or-gate [Wire Wire Wire -> (Val :ok)])
(def or-gate or-gate-3-28)


(ann half-adder [Wire Wire Wire Wire -> (Val :ok)])
(defn half-adder
  [a b s c]
  (let [d (make-wire)
        e (make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s))
  :ok)


(ann full-adder [Wire Wire Wire Wire Wire -> (Val :ok)])
(defn full-adder
  [a b c-in sum c-out]
  (let [s (make-wire)
        c1 (make-wire)
        c2 (make-wire)]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out))
  :ok)


(ann ripple-carry-adder [(NonEmptySeqable Wire)
                         (NonEmptySeqable Wire)
                         (NonEmptySeqable Wire)
                         Wire
                         ->
                         (Val :ok)])
(defn ripple-carry-adder
  "Q. 3.30
  worst case: 2n(and + or)"
  [as bs ss c]
  {:pre [(seq as) (seq bs) (seq ss)]}
  (typed/loop [c-in :- Wire (make-wire)
               as :- (Seqable Wire) as
               bs :- (Seqable Wire) bs
               ss :- (Seqable Wire) ss]
    (let [as-more (seq (rest as))
          bs-more (seq (rest bs))
          ss-more (seq (rest ss))
          a (ignore-with-unchecked-cast (first as) Wire)
          b (ignore-with-unchecked-cast (first bs) Wire)
          s (ignore-with-unchecked-cast (first ss) Wire)]
      (if (and as-more bs-more ss-more)
        (let [c-out (make-wire)]
          (full-adder a b c-in s c-out)
          (recur c-out as-more bs-more ss-more))
        (full-adder a b c-in s c))))
  :ok)


(ann binary-add [(NonEmptySeqable Signal) (NonEmptySeqable Signal) -> (ASeq Signal)])
(defn binary-add [xs ys]
  {:pre [(= (count xs) (count ys))]}
  (let [n (count xs)
        to-wires (typed/fn [x :- Signal] (let [w (make-wire)] (set-signal! w x) w))
        ss (map (fn [_] (make-wire)) (range n))
        c (make-wire)]
    (ignore-with-unchecked-cast (ripple-carry-adder (map to-wires xs) (map to-wires ys) ss c) Any)
    (propagate)
    (cons (get-signal c) (map get-signal ss))))


(ann probe [Any Wire -> (Val :done)])
(defn probe [name wire]
  (add-action!
   wire
   (typed/fn []
     (println)
     (println (str name " " (current-time the-agenda)
                   " new-value = " (get-signal wire)))))
  :done)


; Q. 3.32: skip


(typed/tc-ignore ; experiments


(defn empty-agenda! []
  (propagate)
  (set-car! the-agenda 0))


(defn get-time [n]
  (let [as (map (fn [_] (let [w (make-wire)] (set-signal! w (rand-int 2)) w))
                (range n))
        bs (map (fn [_] (let [w (make-wire)] (set-signal! w (rand-int 2)) w))
                (range n))
        ss (map (fn [_] (make-wire))
                (range n))
        c (make-wire)]
    (ripple-carry-adder as bs ss c)
    (propagate))
  (current-time the-agenda))


(defn random-perf []
  (doseq [n (range 1 21)]
    (let [ts (map (fn [_] (empty-agenda!) (get-time n)) (range 3000))
          t1 (apply min ts)
          t2 (apply max ts)]
      (println (str n " " t1 " " t2)))))

) ; typed/tc-ignore
