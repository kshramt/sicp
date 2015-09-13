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


(declare _eval)
(def _true (symbol "true"))
(def _false (symbol "false"))


(defn error
  ([] (error ""))
  ([msg] (error msg {}))
  ([msg map] (throw (ex-info msg map))))


(defn apply-primitive-procedure [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn compound-procedure? [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn define-variable! [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn extend-environment [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn first-operand [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn lookup-variable-value [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn make-procedure [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn primitive-procedure? [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn procedure-body [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn procedure-environment [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn procedure-parameters [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn set-variable-value! [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn my-true? [& args]
  (throw (Exception. (str "NotImplemented"))))


(defn make-if
  ([predicate consequent]
   (list 'if predicate consequent))
  ([predicate consequent alternative]
   (list 'if predicate consequent alternative)))


(defn make-lambda [parameters body]
  (cons 'lambda (cons parameters body)))


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
    :else (throw (Exception. (str "unknown procedure type -- _apply: " procedure)))))


(let [eval-dispatch-table (make-table)]
  (def lookup-eval-table (eval-dispatch-table :lookup))

  (def insert-eval-table! (eval-dispatch-table :insert!))
  )


(defn _eval
  "Q. 4.3"
  {:test #(do (are [exp env val] (= (_eval exp env) val)
                1 nil 1
                "str" nil "str"
                '(quote (a b)) nil '(a b)))}
  [exp env]
  (cond
    (variable? exp) (lookup-variable-value exp env)
    (self-evaluating? exp) exp
    (sequential? exp) (if-let [impl (lookup-eval-table (first exp))]
                        (impl exp env)
                        (_apply (_eval (operator exp) env)
                                (list-of-values (operands exp) env)))
    :else (error (str "unknown expression type -- _eval: " exp))))


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


