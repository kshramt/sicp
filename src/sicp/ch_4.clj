(ns sicp.ch-4
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer []
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


(declare _eval)
(def _true (symbol "true"))
(def _false (symbol "false"))
(def _nil (symbol "nil"))


(defn error
  ([] (error ""))
  ([msg] (error msg {}))
  ([msg map] (throw (ex-info msg map))))


; begin environment


(def enclosing-environment cdr)
(def first-frame car)
(def the-empty-environment nil)
(defn make-frame "Q. 4.11" [vars vals] (my-map my-cons vars vals))
(defn extend-environment [vars vals base-env]
  (if (= (count vars) (count vals))
    (my-cons (make-frame (apply my-list vars) (apply my-list vals)) base-env)
    (if (< (count vars) (count vals))
      (error (str "Too many arguments supplied: " vars vals))
      (error (str "Too few arguments supplied: " vars vals)))))

(defn frame-variables "Q. 4.11" [frame] (my-map car frame))
(defn frame-values "Q. 4.11" [frame] (my-map cdr frame))
(defn add-binding-to-first-frame! [var val env]
  (set-car! env (my-cons (my-cons var val) (first-frame env))))

(defn lookup-frame
  "Q. 4.12"
  {:test #(do
            (is (nil? (lookup-frame (make-frame (my-list 1 3)
                                                (my-list 2 4))
                                    5)))
            (is (= (lookup-frame (make-frame (my-list 1 3)
                                             (my-list 2 4))
                                 3)
                   (my-list (my-cons 3 4)))))}
  [frame var]
  (cond
    (nil? frame) nil
    (= var (caar frame)) frame
    :else (recur (cdr frame) var)))

(defn lookup-env
  "Q. 4.12"
  {:test #(do
            (is (nil? (lookup-env (my-list (make-frame (my-list 1 2)
                                                       (my-list 3 4)))
                                  5)))
            (is (= (lookup-env (my-list (make-frame (my-list 1 2)
                                                    (my-list 3 4)))
                               2)
                   (my-list (my-cons 2 4)))))}
  [env var]
  (when-not (nil? env)
    (or (lookup-frame (first-frame env) var)
        (recur (enclosing-environment env) var))))

(defn lookup-variable-value
  "Q. 4.16-a"
  {:test #(do
            (let [env (my-list (make-frame (my-list 1 2 :unassigned)
                                           (my-list 3 4 '*unassigned*)))]
              (is (= (lookup-variable-value 1 env) 3))
              (is (= (lookup-variable-value 2 env) 4))
              (is (thrown? clojure.lang.ExceptionInfo
                           (lookup-variable-value :no env)))
              (is (thrown? clojure.lang.ExceptionInfo
                           (lookup-variable-value :unassigned env)))))}
  [var env]
  (if-let [frame (lookup-env env var)]
    (let [ret (cdar frame)]
      (if (= ret '*unassigned*)
        (error (str "Unassigned var in lookup-variable-value: " var))
        ret))
    (error (str "Unbound variable: " var))))

(declare definition?
         make-let
         definition-variable
         definition-value)
(defn scan-out-defines-4-16
  "Q. 4.16-b"
  {:test #(do
            (are [in out] (= in out)
              (scan-out-defines-4-16
               '((define u e1)
                 (define v e2)
                 e3))
              '((let ((u (quote *unassigned*))
                      (v (quote *unassigned*)))
                  (set! u e1)
                  (set! v e2)
                  e3))
              (scan-out-defines-4-16 '(e1 e2))
              '(e1 e2)))}
  [body]
  (let [b (group-by definition? body)]
    (if-let [defs (b true)]
      [(make-let (map (fn [exp] [(definition-variable exp)
                                 '(quote *unassigned*)])
                      defs)
                 (concat (map (fn [d] ['set! (definition-variable d) (definition-value d)])
                              defs)
                         (b false)))]
      (b false) ; as let is lambda, this branch is required to avoid infinite recursion
      )))

(defn scan-out-defines
  "Q. 4.17"
  {:test #(do
            (are [in out] (= in out)
              (scan-out-defines '((define u e1)
                                       e3
                                       (define v e2)))
              '((define u (quote *unassigned*))
                (define v (quote *unassigned*))
                (set! u e1)
                (set! v e2)
                e3)
              (scan-out-defines '(e1 e2))
              '(e1 e2)))}
  [body]
  (let [b (group-by definition? body)]
    (if-let [defs (b true)]
      (concat (map (fn [exp] ['define (definition-variable exp) '(quote *unassigned*)])
                   defs)
              (map (fn [d] ['set! (definition-variable d) (definition-value d)])
                   defs)
              (b false))
      (b false)
      )))

(defn set-variable-value!
  {:test #(do
            (let [env (my-list (make-frame (my-list 1 2)
                                           (my-list 3 4)))]
              (is (thrown? clojure.lang.ExceptionInfo
                           (set-variable-value! :no :no env)))
              (set-variable-value! 1 11 env)
              (is (= env
                     (my-list (make-frame (my-list 1 2)
                                          (my-list 11 4)))))
              (set-variable-value! 2 22 env)
              (is (= env
                     (my-list (make-frame (my-list 1 2)
                                          (my-list 11 22)))))))}
  [var val env]
  (if-let [frame (lookup-env env var)]
    (set-cdr! (car frame) val)
    (error (str "Unbound variable -- set!: " var))))

(defn define-variable!
  {:test #(do
            (let [env (my-list (make-frame (my-list 1 2)
                                           (my-list 3 4)))]
              (define-variable! 1 11 env)
              (is (= env
                     (my-list (make-frame (my-list 1 2)
                                          (my-list 11 4)))))
              (define-variable! 3 33 env)
              (is (= env
                     (my-list (make-frame (my-list 3 1 2)
                                          (my-list 33 11 4)))))
              ))}
  [var val env]
  (if-let [frame (lookup-frame (first-frame env) var)]
    (set-cdr! (car frame) val)
    (add-binding-to-first-frame! var val env))
  var)

(defn -unbind!
  "Q. 4.13"
  {:test #(do
            (let [fr (make-frame (my-list 1 2 3)
                                 (my-list 4 5 6))]
              (is (-unbind! 2 fr))
              (is (= fr
                     (make-frame (my-list 1 3)
                                 (my-list 4 6))))
              (is (not (-unbind! :no fr)))
              (is (-unbind! 3 fr))
              (is (= fr
                     (make-frame (my-list 1)
                                 (my-list 4))))))}
  [var frame]
  (if-let [frame- (cdr frame)]
    (if (= (caar frame-) var)
      (do (set-cdr! frame (cdr frame-))
          true)
      (recur var frame-))
    false))

(defn unbind!
  "Q. 4.13"
  {:test #(do
            (let [env (my-list
                       (make-frame (my-list 1 2 3)
                                   (my-list 4 5 6))
                       (make-frame (my-list 7 8)
                                   (my-list 9 10)))]
              (are [ub e] (do ub (= env e))
                (unbind! :no env)
                (my-list
                 (make-frame (my-list 1 2 3)
                             (my-list 4 5 6))
                 (make-frame (my-list 7 8)
                             (my-list 9 10)))
                (unbind! 2 env)
                (my-list
                 (make-frame (my-list 1 3)
                             (my-list 4 6))
                 (make-frame (my-list 7 8)
                             (my-list 9 10)))
                (unbind! 1 env)
                (my-list
                 (make-frame (my-list 3)
                             (my-list 6))
                 (make-frame (my-list 7 8)
                             (my-list 9 10)))
                (unbind! 7 env)
                (my-list
                 (make-frame (my-list 3)
                             (my-list 6))
                 (make-frame (my-list 8)
                             (my-list 10)))
                (unbind! 3 env)
                (my-list
                 nil
                 (make-frame (my-list 8)
                             (my-list 10)))
                (unbind! 8 env)
                (my-list nil nil))))}
  [var env]
  (when-not (nil? env)
    (if-let [frame (first-frame env)]
      (if (= (caar frame) var)
        (set-car! env (cdr frame))
        (or (-unbind! var frame)
            (recur var (enclosing-environment env))))
      (recur var (enclosing-environment env)))))

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
   ['p_ (fn [& x] (println x) (println (type x)) x)]
   ])

(defn primitive-procedure-names []
  (map first primitive-procedures))

(defn primitive-procedure-objects []
  (map (fn [p] ['primitive (second p)]) primitive-procedures))

(defn setup-environment []
  (let [initial-env (extend-environment (primitive-procedure-names)
                                        (primitive-procedure-objects)
                                        the-empty-environment)]
    (define-variable! _true true initial-env)
    (define-variable! _false false initial-env)
    (define-variable! 'null nil initial-env)
    initial-env))

(def the-global-environment (setup-environment))

; end environment

(def primitive-implementation second)

(defn apply-primitive-procedure [proc args]
  (apply (primitive-implementation proc) args))


(defn tagged-list? [exp tag]
  (and (sequential? exp)
       (= (first exp) tag)))


(defn compound-procedure? [p]
  (tagged-list? p 'procedure))


(defn make-procedure
  "Q. 4.16-c"
  {:test #(do
            (is (= (make-procedure '(a b c)
                                   '((define x a) (define (f x) (+ x b)) (+ x (f b) c))
                                   nil)
                   ['procedure
                    '(a b c)
                    '((define x (quote *unassigned*))
                      (define f (quote *unassigned*))
                      (set! x a)
                      (set! f (lambda (x) (+ x b)))
                      (+ x (f b) c))
                    nil])))}
  [parameters body env]
  ['procedure parameters (scan-out-defines body) env])

(defn primitive-procedure? [proc]
  (tagged-list? proc 'primitive))


(defn procedure-body [p]
  (nth p 2))


(defn procedure-environment [p]
  (nth p 3))


(defn procedure-parameters [p]
  (second p))


(defn my-false? [x]
  (false? x))


(defn my-true? [x]
  (not (my-false? x)))


(defn make-if
  ([predicate consequent]
   (list 'if predicate consequent))
  ([predicate consequent alternative]
   (list 'if predicate consequent alternative)))


(defn make-lambda
  {:test #(do
            (is (= (make-lambda '(x y)
                                '((println x)
                                  (+ x y)))
                   '(lambda (x y) (println x) (+ x y)))))}
  [parameters body]
  (cons 'lambda (cons parameters body)))


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


(defn make-let
  ([pairs body]
   (cons 'let (cons pairs body)))
  ([name pairs body]
   (concat ['let name pairs] body)))


(defn sequence->exp [s]
  (when-let [s (seq s)]
    (if (last-exp? s)
      (first-exp s)
      (make-begin s))))


(defn expand-clauses
  "Q. 4.5"
  [clauses]
  (if-let [clauses (seq clauses)]
    (let [head (first clauses)
          more (rest clauses)]
      (if (cond-else-clause? head)
        (if (seq more)
          (throw (Exception. (str "else clause is not last -- cond->if: " clauses)))
          (sequence->exp (cond-actions head)))
        (if-let [actions (seq (cond-actions head))]
          (let [pred (cond-predicate head)]
            (if (= (first actions) '=>)
              (make-let
               [['v [(make-lambda [] [pred])]]]
               [(make-if
                 'v
                 (if-let [f (second actions)]
                   [f 'v]
                   (throw (Exception.
                           (str "f of pred => f not given -- cond->if: "
                                clauses))))
                 (expand-clauses more))])
              (make-if (cond-predicate head)
                       (sequence->exp (cond-actions head))
                       (expand-clauses more))))
          (throw (Exception. (str "no actions -- cond-> if: " clauses))))))
    _false))


(defn cond->if
  {:test #(do
            (are [in out] (= in out)
              (cond->if '(cond ((ok) 1)
                               ((bad) 2)
                               (else 3)))
              '(if (ok) 1
                   (if (bad) 2
                       3))
              (cond->if '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
                               (else false)))
              '(let ((v ((lambda () (assoc 'b '((a 1) (b 2)))))))
                 (if v
                   (cadr v)
                   false))))}
  [exp]
  (expand-clauses (cond-clauses exp)))


(defn cond? [exp]
  (tagged-list? exp 'cond))


(def rest-operands rest)


(def first-operand first)


(defn no-operands? [ops]
  (nil? (seq ops)))


(def operands next)


(def operator first)


(def application? sequential?)


(def rest-exps rest)


(def begin-actions rest)


(defn begin? [exp]
  (tagged-list? exp 'begin))


(defn if-alternative [exp]
  (if (next (rest (rest exp)))
    (nth exp 3)
    _false))


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
    (make-lambda (rest (second exp))
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


(defn eval-definition
  {:test #(do
            (eval-definition ['define 'x 1] (setup-environment))
            (eval-definition ['define ['f] 1] (setup-environment))
            (eval-definition ['define ['f 'x] 1] (setup-environment))
            (eval-definition ['define ['f 'x 'y] ['+ 'x 'y]] (setup-environment))
            )}
  [exp env]
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
    (_eval (if-consequent exp) env)
    (_eval (if-alternative exp) env)))


(defn list-of-values [exps env]
  (if (no-operands? exps)
    ()
    (cons (_eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))


(defn list-of-values-4-1-lr
  "Q. 4.1"
  [exps env]
  (if (no-operands? exps)
    ()
    (let [l (_eval (first-operand exps) env)
          r (list-of-values-4-1-lr (rest-operands exps) env)]
      (cons l r))))


(defn list-of-values-4-1-rl
  "Q. 4.1"
  [exps env]
  (if (no-operands? exps)
    ()
    (let [r (list-of-values-4-1-rl (rest-operands exps) env)
          l (_eval (first-operand exps) env)]
      (cons l r))))


(defn lookup
  ([table key]
   (when-let [table (seq table)]
     (let [[k v] (first table)]
       (if (= k key)
         v
         (recur (rest table) key))))))


(defn insert [table key value]
  (if-let [table (seq table)]
    (let [[ k v :as kv] (first table)]
      (if (= k key)
        (cons [k value] (rest table))
        (cons kv (insert (rest table) key value))))
    (cons [key value] nil)))


(defn make-table
  {:test #(let [table (make-table)]
            (is (nil? ((table :lookup) :a)))
            ((table :insert!) 'a 1)
            (is (= ((table :lookup) 'a) 1)))}
  []
  (let [local-table (atom nil)]
    (letfn [(dispatch [m]
              (case m
                :lookup (fn [t] (lookup @local-table t))
                :insert! (fn [t f] (swap! local-table insert t f))
                (throw (Exception. (str "unknown operation for table: " m)))))]
      dispatch)))


(defn let->combination
  "
  Q 4.6
  Q 4.8
  "
  {:test #(do (are [in out] (= in out)
                (let->combination '(let ()))
                '((lambda ()))
                (let->combination '(let () 1 2))
                '((lambda () 1 2))
                (let->combination '(let ((a 1) (b 2)) (+ a b)))
                '((lambda (a b) (+ a b)) 1 2)
                (let->combination '(let ((a 1) (b 2)) (print a) (+ a b)))
                '((lambda (a b) (print a) (+ a b)) 1 2)
                (let->combination '(let impl ((a 1) (b 2)) (print a) (impl a b)))
                '(let () (define (impl a b) (print a) (impl a b)) (impl 1 2))
                )
              (is (thrown? Exception (let->combination '(let))))
              )}
  [exp]
  (if-let [args (next exp)]
    (if (variable? (first args))
      (let [f (first args)
            pairs (second args)
            names (map first pairs)
            vals (map second pairs)
            body (rest (rest args))]
        (make-let [] [(concat ['define (cons f names)] body) (cons f vals)]))
      (let [pairs (first args)
            names (map first pairs)
            vals (map second pairs)
            body (rest args)]
        (cons (make-lambda names body)
              vals)))
    (throw (Exception. (str "No argumets are given for let: " exp)))))


(defn let*->nested-lets
  "Q 4.7"
  {:test #(do (are [in out] (= in out)
                (let*->nested-lets '(let* ()))
                '(let ())
                (let*->nested-lets '(let* ((a 1) (a 2)) b c))
                '(let ((a 1)) (let ((a 2)) (let () b c)))
                )
              (is (thrown? Exception (let*->nested-lets '(let))))
              )}
  [exp]
  (if-let [args (next exp)]
    (let [body (rest args)]
      (letfn [(expand [pairs]
                (if-let [pairs (seq pairs)]
                  (make-let [(first pairs)] [(expand (rest pairs))])
                  (make-let [] body)))]
        (expand (first args))))
    (throw (Exception. (str "No argumets are given for let*: " exp)))))


(let [id (atom 0)]
  (defn my-gensym []
    (symbol (str "__GENSYM_" (swap! id inc)))))


(defn eval-and-special
  "Q. 4.4"
  {:test #(do
            (is (= (eval-and-special '(and) nil) _true)))}
  [exp env]
  (loop [s (operands exp)
         ret _true]
    (if-let [s (seq s)]
      (let [ret (_eval (first s) env)]
        (if (my-true? ret)
          (recur (rest s) ret)
          _false))
      ret)))


(defn wrap-by-lambda [xs]
  (map (fn [fi v]
         [(symbol (str "f" fi))
          (make-lambda [] [v])])
       (range)
       xs))


(defn expand-and
  "Q. 4.4"
  {:test #(do
            (are [in out] (= in out)
              (expand-and '(and))
              _true
              (expand-and '(and 1))
              '(let ((f0 (lambda () 1)))
                 (let ((v (f0)))
                   (if v v)))
              (expand-and '(and (a b) (c d)))
              '(let ((f0 (lambda () (a b)))
                     (f1 (lambda () (c d))))
                 (let ((v (f0)))
                   (if v
                     (let ((v (f1)))
                       (if v v)))))))}
  [exp]
  (if-let [args (operands exp)]
    (letfn [(expand [s]
              (make-let
               [['v (first s)]]
               [(if-let [more (next s)]
                  (make-if 'v (expand more))
                  (make-if 'v 'v))]))]
      (let [fs (wrap-by-lambda args)]
        (make-let fs [(expand (map (fn [kv] [(first kv)]) fs))])))
    _true))


(defn eval-and-derived
  "Q. 4.4"
  [exp env]
  (_eval (expand-and exp) env))


(defn eval-or-special
  "Q. 4.4"
  {:test #(do
            (is (= (eval-or-special '(or) nil) _false)))}
  [exp env]
  (loop [s (operands exp)]
    (if-let [s (seq s)]
      (let [ret (_eval (first s) env)]
        (if (my-true? ret)
          ret
          (recur (rest s))))
      _false)))


(defn expand-or
  "Q. 4.4"
  {:test #(do
            (are [in out] (= in out)
              (expand-or '(or))
              _false
              (expand-or '(or 1))
              '(let ((f0 (lambda () 1)))
                 (let ((v (f0)))
                   (if v v)))
              (expand-or '(or (a b) (c d)))
              '(let ((f0 (lambda () (a b)))
                     (f1 (lambda () (c d))))
                 (let ((v (f0)))
                   (if v v
                     (let ((v (f1)))
                       (if v v)))))))}
  [exp]
  (if-let [args (operands exp)]
    (letfn [(expand [s]
              (make-let
               [['v (first s)]]
               [(if-let [more (next s)]
                  (make-if 'v 'v (expand more))
                  (make-if 'v 'v))]))]
      (let [fs (wrap-by-lambda args)]
        (make-let fs [(expand (map (fn [kv] [(first kv)]) fs))])))
    _false))


(defn eval-or-derived
  "Q. 4.4"
  [exp env]
  (_eval (expand-or exp) env))


(defn expand-while
  "Q. 4.9"
  {:test #(do
            (are [in out] (= in out)
              (expand-while '(while (> i 10) (print i) (set! i (- i 1))))
              '(let ((pred (lambda () (> i 10)))
                     (body (lambda () (print i) (set! i (- i 1)))))
                 (let loop ()
                      (if (pred)
                        (begin (body)
                               (loop))))))
            (is (thrown? clojure.lang.ExceptionInfo (expand-while '(while))))
            (is (thrown? clojure.lang.ExceptionInfo (expand-while '(while true)))))}
  [exp]
  (if (> (count exp) 2)
    (let [[_ pred & body] exp]
      (make-let [['pred (make-lambda [] [pred])]
                 ['body (make-lambda [] body)]]
                [(make-let 'loop
                           []
                           ['(if (pred)
                               (begin (body)
                                      (loop)))])]))
    (error (str "(while pred op_1 op_2 ...)" exp))))


(defn _apply [procedure arguments]
  (cond
    (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
    (compound-procedure? procedure) (eval-sequence
                                     (procedure-body procedure)
                                     (extend-environment
                                      (procedure-parameters procedure)
                                      arguments
                                      (procedure-environment procedure)))
    :else (error (str "unknown procedure type -- _apply: " (type procedure) " :: " procedure))))


(let [eval-dispatch-table (make-table)]
  (def lookup-eval-table (eval-dispatch-table :lookup))

  (def insert-eval-table! (eval-dispatch-table :insert!))
  )

(defn symbolize-nil-true-false [exp]
  (if (sequential? exp)
    (map symbolize-nil-true-false exp)
    (case exp
      true _true
      false _false
      nil _nil
      exp)))

(defn _eval
  "Q. 4.3"
  {:test #(do (are [exp env val] (= (_eval (symbolize-nil-true-false exp) env) val)
                1 nil 1
                "str" nil "str"
                '(quote (a b)) nil '(a b)
                '(define f 1) (setup-environment) 'f
                '(define (f x) x) (setup-environment) 'f
                '(begin 1 2) (setup-environment) 2
                '(begin (define (f x) x) (f 8)) (setup-environment) 8
                '(begin (define (f x y) (+ x y)) (f 8 9)) (setup-environment) 17
                '(if true 1 2) (setup-environment) 1
                '(if false 1 2) (setup-environment) 2
                '(if (null? null) 1 2) (setup-environment) 1
                '(if (null? true) 1 2) (setup-environment) 2
                '(begin (define (append x y)
                          (if (null? x)
                            y
                            (cons (car x)
                                  (append (cdr x)
                                          y))))
                        (append (list 1 2 3) (list 4 5 6)))
                (setup-environment)
                (_eval '(list 1 2 3 4 5 6) (setup-environment))
                '(begin (define (factorial n)
                          (define (impl k ret)
                            (if (= k 1)
                              ret
                              (impl ( - k 1) (* k ret))))
                          (impl n 1))
                        (factorial 5))
                (setup-environment)
                120
                ))}
  [exp env]
  (cond
    (variable? exp) (lookup-variable-value exp env)
    (self-evaluating? exp) exp
    (sequential? exp) (if-let [impl (lookup-eval-table (first exp))]
                        (impl exp env)
                        (_apply (_eval (operator exp) env)
                                (list-of-values (operands exp) env)))
    :else (error (str "unknown expression type -- _eval: " (type exp) " :: " exp))))


(insert-eval-table! 'quote (fn [exp env] (text-of-quotation exp)))
(insert-eval-table! 'set! eval-assignment)
(insert-eval-table! 'define eval-definition)
(insert-eval-table! 'if eval-if)
(insert-eval-table! 'lambda (fn [exp env] (make-procedure (lambda-parameters exp)
                                                          (lambda-body exp)
                                                          env)))
(insert-eval-table! 'begin (fn [exp env] (eval-sequence (begin-actions exp) env)))
(insert-eval-table! 'cond (fn [exp env] (_eval (cond->if exp) env)))
(insert-eval-table! 'let (fn [exp env] (_eval (let->combination exp) env)))
(insert-eval-table! 'let* (fn [exp env] (_eval (let*->nested-lets exp) env)))
(insert-eval-table! 'and eval-and-derived)
(insert-eval-table! 'or eval-or-derived)
(insert-eval-table! 'while (fn [exp env] (_eval (expand-while exp) env))) ; Q. 4.10


; begin repl

(def input-prompt ";;; M-Eval input:")
(def output-prompt ";;; M-Eval value:")

(def prompt-for-input println)
(def announce-output println)


(defn user-print [object]
  (if (compound-procedure? object)
    (println (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)))
    (println object))
  (flush))

(defn driver-loop []
  (prompt-for-input input-prompt)
  (let [input (symbolize-nil-true-false (read))]
    (if (= input 'quit)
      :done
      (let [output (_eval input the-global-environment)]
        (println (str "input :: " (type input)))
        (announce-output output-prompt)
        (println (str "output :: " (type output)))
        (user-print output)
        (recur)))))

; end REPL


; Q. 4.14 skip
; Q. 4.15 skip

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


