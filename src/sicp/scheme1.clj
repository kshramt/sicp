(ns sicp.scheme1
  (:require
   [clojure.test :refer [is are deftest]]
   [sicp.pair
    :refer [
            List
            any?
            car
            caadr
            caar
            cadr
            caddr
            cadddr
            cdadr
            cdar
            cdr
            cddr
            cddr
            cdddr
            my-cons
            my-list
            my-map
            pair?
            set-car!
            set-cdr!
            ]
    ]
   [sicp.util
    :refer [
            p_
            ]]
   )
  )

(declare procedure?
         procedure-parameters
         procedure-body
         primitive?)

(defn user-str [object]
  (str (if (procedure? object)
         ['compound-procedure
          (procedure-parameters object)
          (procedure-body object)]
         object)))

(defn str-frame [frame]
  (when-let [[k v] (first frame)]
    (when-not (primitive? v)
      (str k "\t" (user-str v) "\n"
           (str-frame (rest frame))))))

(declare current
         enclosing)
(defn str-env [env]
  (if env
    (str "^^^^^^^^^^^^^^\n"
         (str-frame (current env))
         (str-env (enclosing env)))
    "=============="))

(defprotocol IEnv
  (current [env])
  (enclosing [env])
  )
(deftype Env [_current _enclosing]
 IEnv
 (current [self] _current)
 (enclosing [self] _enclosing)
 Object
 (toString [self] (str-env self))
)

(defn error
  ([] (error ""))
  ([msg] (error msg {}))
  ([msg map] (throw (ex-info msg map))))

(declare _eval
         _apply
         )

(def _nil (symbol "nil"))
(def _true (symbol "true"))
(def _false (symbol "false"))
(def null nil)

(defn symbolize [exp]
  (cond
    (nil? exp) _nil
    (true? exp) _true
    (false? exp) _false
    (sequential? exp) (map symbolize exp)
    :else exp
    ))

(defn make-frame
  "Assumption: no parallel access to environment"
  [vars vals]
  (let [m (java.util.HashMap.)]
    (loop [vars (seq vars)
           vals (seq vals)]
      (if (and vars vals)
        (do
          (.put m (first vars) (first vals))
          (recur (next vars)
                 (next vals)))
        m))))

(defn self-evaluating? [exp]
  (or (number? exp)
      (string? exp)))

(def variable? symbol?)

(defn tagged-list? [tag exp]
  (and (sequential? exp)
       (= (first exp) tag)))

(defmacro make-pred [sym]
  `(defn ~(symbol (str sym "?")) [exp#]
     (tagged-list? (quote ~sym) exp#)))

(make-pred quote)
(make-pred set!)
(make-pred define)
(make-pred if)
(make-pred lambda)
(make-pred begin)
(make-pred cond)
(make-pred let)
(make-pred let*)
(make-pred letrec)
(make-pred and)
(make-pred or)
(make-pred procedure)
(make-pred primitive)

(defn user-print [object]
  (println (user-str object)))

(defn print-env [env]
  ; use str-env to print nil properly
  (println (str-env env)))

(defn _print-env [env]
  (print-env env)
  env)

(def my-false? false?)
(defn my-true? [x]
  (not (my-false? x)))

(defn make-begin [s]
  (cons 'begin s))

(defn sequence->exp [s]
  (if (next s)
    (make-begin s)
    (first s)))

(defn make-let
  ([pairs body] (cons 'let (cons pairs body)))
  ([name pairs body] (cons 'let (cons name (cons pairs body)))))

(defn make-lambda [params body]
  (cons 'lambda (cons params body)))

(defn make-if
  ([pred then] ['if pred then])
  ([pred then else] ['if pred then else]))

(declare definition-variable
         definition-value)
(defn scan-out-defines [body]
  (let [b (group-by define? body)
        defs (b true)]
    (concat (map (fn [d] ['define (definition-variable d) '(quote *unassigned*)]) defs)
            (map (fn [d] ['set! (definition-variable d) (definition-value d)]) defs)
            (b false))))

(defn make-procedure [params body env]
    ['procedure params (scan-out-defines body) env])

(defn cond->if [exp]
  (letfn [(expand [exp]
            (if exp
              (let [[head & more] exp]
                (if (= (first head) 'else)
                  (if more
                    (error (str "else clauses is not last -- cond->if: " exp))
                    (sequence->exp (rest head)))
                  (let [[pred & actions] head]
                    (if actions
                      (if (= (first actions) '=>)
                        (if (= (count actions) 2)
                          (make-let [['v pred]
                                     ['more (make-lambda [] [(expand more)])]]
                                    [(make-if 'v
                                              [(second actions) 'v]
                                              ['more])])
                          (error (str "too few/many actions for => -- cond->if: " exp)))
                        (make-if pred
                                 (sequence->exp actions)
                                 (expand more)))
                      (make-let [['v pred]
                                 ['more (make-lambda [] [(expand more)])]]
                                [(make-if 'v 'v ['more])])))))
              _false))]
    (expand (next exp))))

(def if-predicate second)
(defn if-consequent [exp]
  (nth exp 2))
(defn if-alternative [exp]
  (nth exp 3 _false))

(defn let->combination [[_ pairs & body]]
  (let [vars (map first pairs)
        vals (map second pairs)]
    (cons (make-lambda vars body) vals)))

(defn let*->nested-lets [[_ pairs & body]]
  (letfn [(expand [[head & more]]
            (if more
              (make-let [head]
                        [(expand more)])
              (make-let [head] body)))]
    (if (seq pairs)
      (expand pairs)
      body)))

(defn expand-letrec [[_ pairs & body :as exp]]
  (if pairs
    (make-let (map (fn [p] [(first p) '(quote *unassigned*)]) pairs)
              (concat (map (fn [p] ['set! (first p) (second p)]) pairs)
                      body))
    (error (str "No decl nor body provided -- letrec: " exp))))

(defn define-variable! [var val env]
  (let [^java.util.HashMap f (current env)]
    (.put f var val)
    var))

(defn definition-variable [exp]
  (let [var (second exp)]
    (if (symbol? var)
      var
      (first var))))

(defn definition-value [exp]
  (let [var (second exp)]
    (if (symbol? var)
      (nth exp 2)
      (make-lambda (rest var)
                   (nnext exp)))))

(defn lookup-env [var env]
  (when env
    (let [^java.util.HashMap f (current env)]
      (if (.containsKey f var)
        f
        (recur var (enclosing env))))))

(defn set-variable-value! [var val env]
  (if-let [^java.util.HashMap m (lookup-env var env)]
    (.put m var val)
    (error (str "Unbound variable -- set!: " var))))

(defn lookup-variable-value
  [var env]
  (if-let [^java.util.HashMap m (lookup-env var env)]
    (let [ret (.get m var)]
      (if (= ret '*unassigned*)
        (error (str "Unassigned var -- lookup-variable-value: " var))
        ret))
    (error (str "Unbound variable: " var))))

(def primitive-implementation second)
(defn apply-primitive-procedure [proc args]
  (apply (primitive-implementation proc) args))

(defn eval-sequence [exps env]
  (if (seq exps)
    (loop [[head & more] exps]
      (if more
        (do (_eval head env)
            (recur more))
        (_eval head env)))
    (error (str "Empty exps -- eval-sequence"))))

(defn procedure-body [p]
  (nth p 2))

(defn procedure-environment [p]
  (nth p 3))

(defn procedure-parameters [p]
  (second p))

(defn extend-environment [vars vals base-env]
  (if (= (count vars) (count vals))
    (Env. (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (error (str "Too many arguments supplied: " vars vals))
      (error (str "Too few arguments supplied: " vars vals)))))

(def primitive-procedures
  [['null? nil?]
   ['pair? pair?]
   ['car car]
   ['cdr cdr]
   ['cons my-cons]
   ['list my-list]
   ['+ +]
   ['- -]
   ['* *]
   ['/ /]
   ['= =]
   ['< <]
   ['> >]
   ['<= <=]
   ['>= >=]
   ['eq? =] ; todo:
   ['symbol? symbol?]
   ['number? number?]
   ['string? string?]
   ['print print]
   ['println println]
   ['str str]
   ['error error]
   ])

(defn primitive-procedure-names []
  (map first primitive-procedures))

(defn primitive-procedure-objects []
  (map (fn [p] ['primitive (second p)]) primitive-procedures))

(defn make-emtpy-environment [] nil)

(defn setup-environment []
  (let [initial-env (extend-environment (primitive-procedure-names)
                                        (primitive-procedure-objects)
                                        (make-emtpy-environment))]
    (define-variable! _true true initial-env)
    (define-variable! _false false initial-env)
    (define-variable! 'null nil initial-env)
    initial-env))

(defn _apply [proc args]
  (cond
    (primitive? proc) (apply-primitive-procedure proc args)
    (procedure? proc) (eval-sequence
                       (procedure-body proc)
                       (extend-environment
                        (procedure-parameters proc)
                        args
                        (procedure-environment proc)))
    :else (error (str "Unknown procedure type -- _apply: " (type proc) " :: " proc))))

(defn _eval
  [exp env]
  ;(user-print exp)
  ;(print-env env)
  (if (sequential? exp)
    (if-let [op (first exp)] ; returns null for ()
      (case op
        quote (second exp)
        set! (set-variable-value!
              (second exp)
              (_eval (nth exp 2) env)
              env)
        define (define-variable!
                 (definition-variable exp)
                 (_eval (definition-value exp) env)
                 env)
        if (if (my-true? (_eval (if-predicate exp) env))
             (recur (if-consequent exp) env)
             (recur (if-alternative exp) env))
        lambda (make-procedure (second exp)
                               (nnext exp)
                               env)
        begin (eval-sequence (next exp) env)
        cond (recur (cond->if exp) env)
        let (recur (let->combination exp) env)
        let* (recur (let*->nested-lets exp) env)
        letrec (recur (expand-letrec exp) env)
        and (if-let [args (next exp)]
              (loop [[head & more] args]
                (let [v (_eval head env)]
                  (if more
                    (if (my-true? v)
                      (recur more)
                      false)
                    (if (my-true? v)
                      v
                      false))))
              true)
        or (if-let [args (next exp)]
             (loop [[head & more] args]
               (let [v (_eval head env)]
                 (if more
                   (if (my-true? v)
                     v
                     (recur more))
                   (if (my-true? v)
                     v
                     false))))
             false)
        (_apply (_eval (first exp) env)
                (map #(_eval % env) (rest exp)))
        )
      null)
    (cond
      (self-evaluating? exp) exp
      (variable? exp) (lookup-variable-value exp env)
      :else (error (str "Unknown expression type -- _eval: " (type exp) " :: " exp)))))

(def input-prompt ";;; M-Eval input:")
(def output-prompt ";;; M-Eval value:")

(def prompt-for-input println)
(def announce-output println)

(defn driver-loop []
  (let [env (setup-environment)]
    (loop []
      (prompt-for-input input-prompt)
      (let [input (symbolize (read))]
        (println (str "input: " (type input) " :: " input))
        (if (= input 'quit)
          :done
          (let [output (_eval input env)]
            (announce-output output-prompt)
            (println (str "output: " (type output)))
            (user-print output)
            (recur)))))))

(deftest test-_eval
  (are [exp val] (= (_eval (symbolize exp) (setup-environment)) val)
    1 1
    "str" "str"
    '(quote (a b)) '(a b)
    '(define f 1) 'f
    '(define (f x) x) 'f
    '(begin 1 2) 2
    '(begin (define (f x) x) (f 8)) 8
    '(begin (define (f x y) (+ x y)) (f 8 9)) 17
    '(if true 1 2) 1
    '(if false 1 2) 2
    '(if (null? null) 1 2) 1
    '(if (null? true) 1 2) 2
    '(or) false
    '(or false 1 2) 1
    '(and) true
    '(and false 1 2) false
    '(and 1 2) 2
    '(and 1 2 false) false
    '(begin (define (append x y)
              (if (null? x)
                y
                (cons (car x)
                      (append (cdr x)
                              y))))
            (append (list 1 2 3) (list 4 5 6)))
    (_eval '(list 1 2 3 4 5 6) (setup-environment))
    '(begin (define (factorial n)
              (define (impl k ret)
                (if (= k 1)
                  ret
                  (impl (- k 1) (* k ret))))
              (impl n 1))
            (factorial 5))
    120
    '(letrec ((fact
               (lambda (n)
                       (if (= n 1)
                         1
                         (* n (fact (- n 1)))))))
             (fact 10))
    3628800
    '((lambda (n) ; Q. 4.21-a
              ((lambda (fib)
                       (fib fib n))
               (lambda (fb n)
                       (if (< n 2)
                         1
                         (+ (fb fb (- n 1))
                            (fb fb (- n 2)))))))
      6)
    13
    '(cond (false 0) (1)) 1
    ))
