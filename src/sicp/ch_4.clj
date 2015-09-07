(ns sicp.ch-4
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [All
            Any
            ASeq
            CountRange
            ExactCount
            I
            IFn
            Int
            Kw
            NonEmptySeqable
            Num
            Option
            Rec
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
   [sicp.pair
    :refer [
            List
            any?
            car
            caadr
            cadr
            caddr
            cadddr
            cdadr
            cdr
            cddr
            cddr
            cdddr
            my-cons
            my-list
            pair?
            set-car!
            set-cdr!
            ]
    ]
   [clojure.math.numeric-tower
    :refer
    [
     abs
     gcd
     sqrt
     ]]
   [sicp.util
    :refer [
            p_
            pef
            ]]
   )
  (:import [sicp.pair Pair]))


(declare
 apply-primitive-procedure
 compound-procedure?
 define-variable!
 extend-environment
 first-operand
 lookup-variable-value
 make-procedure
 primitive-procedure?
 procedure-body
 procedure-environment
 procedure-parameters
 set-variable-value!
 my-true?
 )


(declare _eval)


(defn make-if [predicate consequent alternative]
  (my-list 'if predicate consequent alternative))


(defn make-lambda [parameters body]
  (my-cons 'lambda (my-cons parameters body)))


(defn tagged-list? [exp tag]
  (and (sequential? exp)
       (= (first exp) tag)))


(def cond-actions rest)


(def cond-predicate first)


(def cond-clauses rest)


(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))


(defn make-begin [s]
  (cons 'begin s))


(def first-exp first)


(defn last-exp? [s]
  (nil? (next s)))


(defn sequence->exp [s]
  (when-let [s (seq s)]
    (if (last-exp? s)
      (first-exp s)
      (make-begin s))))


(defn expand-clauses [clauses]
  (if-let [clauses (seq clauses)]
    (let [head (first clauses)
          more (rest clauses)]
      (if (cond-else-clause? head)
        (if (seq more)
          (throw (Exception. (str "else clause is not last -- cond->if: " clauses)))
          (sequence->exp (cond-actions head)))
        (make-if (cond-predicate head)
                 (sequence->exp (cond-actions head))
                 (expand-clauses more))))
    'false))


(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))


(defn cond? [exp]
  (tagged-list? exp 'cond))


(def rest-operands rest)


(def first-operand first)


(defn no-operands? [ops]
  (nil? (seq ops)))


(def operands rest)


(def operator first)


(def application? sequential?)


(def rest-exps rest)


(def begin-actions rest)


(defn begin? [exp]
  (tagged-list? exp 'begin))


(defn if-alternative [exp]
  (if (next (rest (rest exp)))
    (nth exp 3)
    'false))


(defn if-consequent [exp]
  (nth exp 2))


(def if-predicate second)


(defn if? [exp]
  (tagged-list? exp 'if))


(defn lambda-body [exp]
  (rest (rest exp)))


(def lambda-parameters second)


(defn lambda? [exp]
  (tagged-list? exp 'lambda))


(defn definition-value [exp]
  (if (symbol? (second exp))
    (nth exp 2)
    (make-lambda (second (rest exp))
                 (rest (rest exp)))))


(defn definition-variable [exp]
  (if (symbol? (second exp))
    (second exp)
    (first (second exp))))


(defn definition? [exp]
  (tagged-list? exp 'define))


(defn assignment-value [exp]
  (nth exp 2))


(def assignment-variable second)


(defn assignment? [exp]
  (tagged-list? exp 'set!))


(def text-of-quotation second)


(defn quoted? [exp]
  (tagged-list? exp 'quote))


(def variable? symbol?)


(defn self-evaluating? [exp]
  (or (number? exp)
      (string? exp)))


(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
                    (_eval (definition-value exp) env)
                    env))


(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (_eval (definition-value exp) env)
                       env))


(defn eval-sequence [exps env]
  (cond (last-exp? exps) (_eval (first-exp exps) env)
        :else (do (_eval (first-exp exps) env)
                  (eval-sequence (rest-exps exps) env))))


(defn eval-if [exp env]
  (if (my-true? (_eval (if-predicate exp) env))
    (_eval (if-consequent exp) eval)
    (_eval (if-alternative exp) env)))


(defn list-of-values [exps env]
  (if (no-operands? exps)
    ()
    (my-cons (_eval (first-operand exps) env)
             (list-of-values (rest-operands exps) env))))


(defn _apply [procedure arguments]
  (cond
    (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
    (compound-procedure? procedure) (eval-sequence
                                     (procedure-body procedure)
                                     (extend-environment
                                      (procedure-parameters procedure)
                                      arguments
                                      (procedure-environment procedure)))
    :else (throw (Exception. (str "unknown procedure type -- _applly: " procedure)))))


(defn _eval [exp env]
  (cond
    (self-evaluating? exp) exp
    (variable? exp) (lookup-variable-value exp env)
    (quoted? exp) (text-of-quotation exp)
    (assignment? exp) (eval-assignment exp env)
    (definition? exp) (eval-definition exp env)
    (if? exp) (eval-if exp env)
    (lambda? exp) (make-procedure (lambda-parameters exp)
                                  (lambda-body exp)
                                  env)
    (begin? exp) (eval-sequence (begin-actions exp) env)
    (cond? exp) (recur (cond->if exp) env)
    (application? exp) (_apply (_eval (operator exp) env)
                               (list-of-values (operands exp) env))
    :else (throw (Exception. (str "unknown expression type -- _eval: " exp)))))


;; Q. 4.2-a (define x 3) -> application


(defn operands-4-2-b [exp]
  (rest (rest exp)))


(defn operator-4-2-b [exp]
  (first (rest exp)))


(defn application?-4-2-b [exp]
  (tagged-list? exp 'call))


(defn _eval-4-2-b
  "Q. 4.2-b"
  [exp env]
  (cond
    (application?-4-2-b exp) (_apply (_eval (operator-4-2-b exp) env)
                                     (list-of-values (operands-4-2-b exp) env))
    (self-evaluating? exp) exp
    (variable? exp) (lookup-variable-value exp env)
    (quoted? exp) (text-of-quotation exp)
    (assignment? exp) (eval-assignment exp env)
    (definition? exp) (eval-definition exp env)
    (if? exp) (eval-if exp env)
    (lambda? exp) (make-procedure (lambda-parameters exp)
                                  (lambda-body exp)
                                  env)
    (begin? exp) (eval-sequence (begin-actions exp) env)
    (cond? exp) (recur (cond->if exp) env)
    :else (throw (Exception. (str "unknown expression type -- _eval-4-2-b: " exp)))))
