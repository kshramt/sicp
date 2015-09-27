; eval & apply
(define (_eval exp env)
  ;(user-print (list "EXP: " exp "\n"))
  (cond
   ((variable? exp) (lookup-variable-value exp env))
   ((self-evaluating? exp) exp)
   ((quote? exp) (text-of-quotation exp))
   ((set!? exp) (set-variable-value! (set!-variable exp)
                                     (_eval (set!-value exp) env)
                                     env))
   ((define? exp) (eval-definition (definition-variable exp)
                                   (_eval (definition-value exp) env)
                                   env))
   ((if? exp) (if (my-true? (_eval (if-predicate exp) env))
                  (_eval (if-consequent exp) env)
                  (_eval (if-alternative exp) env)))
   ((lambda? exp) (make-procedure (lambda-parameters exp)
                                  (lambda-body exp)
                                  env))
   ((begin? exp) (eval-sequence (operands exp) env))
   ((cond? exp) (_eval (cond->if exp) env))
   ((let? exp) (_eval (let->lambda exp) env))
   ((let*? exp) (_eval (let*->nested-let exp) env))
   ((letrec? exp) (_eval (expand-letrec exp) env))
   ((and? exp) (_eval (expand-and exp) env))
   ((or? exp) (_eval (expand-or exp) env))
   ((pair? exp) (_apply (_eval (operator exp) env)
                        (list-of-values (operands exp) env)))
   (else (error (str "Unsupported expression -- _eval: " exp)))))

(define (p_ x)
  (user-print (list "P_\t" x "\n"))
  x)

(define (_apply proc args)
  (cond
   ((primitive? proc) (apply-primitive-procedure proc args))
   ((procedure? proc) (eval-sequence
                       (procedure-body proc)
                       (extend-environment
                        (procedure-parameters proc)
                        args
                        (procedure-environment proc))))
   (else (error (str "Unknown procedure type -- scheme1.scm/_apply: " proc)))))


; REPL
(define (driver-loop env fp)
  (let ((fp (%pushback-reader fp)))
    (let loop ()
      (print input-prompt "\n")
      (let ((input (read fp)))
        (user-print (list input "\n"))
        (if (or (eq? input 'quit) (eq? input EOF))
            'bye
            (let ((output (_eval input env)))
              (print output-prompt "\n")
              (user-print (list output "\n"))
              (loop)))))))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (print-env env)
  (print (str-env env) "\n"))

(define (str-env env)
  (if (null? env)
      "===="
      (str "^^^^\n"
           (str-frame (current env))
           (str-env (enclosing env)))))

(define (str-frame frame)
  (let loop ((vars (variables frame))
             (vals (values frame)))
    (if (null? vars)
        ""
        (let ((k (car vars))
              (v (cdr vals)))
          (str (if (primitive? v)
                   ""
                   (str k "\t" (user-str (list v)) "\n"))
               (loop (cdr (vars)) (cdr vals)))))))

; base environment
(define (user-str objects)
  (define (conv o)
    (str (if (procedure? o)
             (list 'compound-procedure
                   (procedure-parameters o)
                   (procedure-body o))
             o)))
  (trans/foldl (trans/map conv) str "" objects))

(define (user-print objects)
  (print (user-str objects)))

(define (setup-environment)
  (let ((initial-env (extend-environment
                      (primitive-procedure-names)
                      (primitive-procedure-objects)
                      the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-empty-environment ())
(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects) (map (lambda (p) (list 'primitive (cdr p)))
                                           primitive-procedures))
(define primitive-procedures
  (list
   (cons 'apply _apply)
   (cons 'null? null?)
   (cons 'pair? pair?)
   (cons 'car car)
   (cons 'cdr cdr)
   (cons 'set-car! set-car!)
   (cons 'set-cdr! set-cdr!)
   (cons 'cons cons)
   (cons 'list list)
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons '= =)
   (cons '< <)
   (cons '> >)
   (cons 'eq? eq?)
   (cons 'false? false?)
   (cons 'true? true?)
   (cons 'symbol? symbol?)
   (cons 'number? number?)
   (cons 'string? string?)
   (cons 'read read)
   (cons 'current-input-port current-input-port)
   (cons 'open-input-file open-input-file)
   (cons '%pushback-reader %pushback-reader)
   (cons '__print user-print)
   (cons '__str user-str)
   (cons 'error error)
   ))


; internals
(define (make-frame vars vals)
  (cons vars vals))
(define variables car)
(define values cdr)

(define (make-begin s) (cons 'begin s))

(define (sequence->exp s)
  (if (null? (cdr s))
      (car s)
      (make-begin s)))

(define (make-procedure params body env)
  (list 'procedure params (scan-out-defines body) env))

(define (scan-out-defines body)
  (let* ((b (foldl (lambda (acc e)
                     (if (define? e)
                         (cons (cons e (car acc))
                               (cdr acc))
                         (cons (car acc)
                               (cons e (cdr acc)))))
                   (cons () ())
                   body))
         (defs (reverse (car b)))
         (body (reverse (cdr b))))
    (concat (map (lambda (d) (list 'define (definition-variable d) '(quote *unassigned*))) defs)
            (map (lambda (d) (list 'set! (definition-variable d) (definition-value d))) defs)
            body)))

(define (cond->if exp)
  (let expand ((exp (cdr exp)))
    (if (null? exp)
        false
        (let ((head (car exp))
              (more (cdr exp)))
          (if (eq? (car head) 'else)
              (if (null? more)
                  (sequence->exp (cdr head))
                  (error (str "else clauses is not last -- cond->if: " exp)))
              (let ((pred (car head))
                    (actions (cdr head)))
                (if (null? actions)
                    (list 'let (list (list 'v pred)
                                     (list 'more (list 'lambda () (expand more))))
                          (list 'if 'v 'v (list 'more)))
                    (if (eq? (car actions) '=>)
                        (let ((f (cdr actions)))
                          (if (null? f)
                              (error (str "cond => has no f: " exp))
                              (if (null? (cdr f))
                                  (list 'let (list (list 'v pred)
                                                   (list 'more (list 'lambda () (expand more))))
                                        (list 'if 'v ((car f) 'v) (list 'more)))
                                  (error (str "more than 1 f for cond =>: " exp)))))
                        (list 'if
                              pred
                              (sequence->exp actions)
                              (expand more))))))))))

(define (let->lambda exp)
  (let ((pairs (cadr exp))
        (body (cddr exp)))
    (let ((vars (map car pairs))
          (vals (map cadr pairs)))
      (cons (list 'lambda vars body) vals))))

(define (let*->nested-let exp)
  (let ((pairs (cadr exp))
        (body (cddr exp)))
    (if (null? pairs)
        (cons 'let (cons () body))
        (let expand ((head (car pairs))
                     (more (cdr pairs)))
          (if (null? more)
              (cons 'let (cons (list head) body))
              (list 'let (list head) (expand more)))))))

(define (expand-letrec exp)
  (let ((pairs (cadr exp))
        (body (cddr exp)))
    (cons 'let (cons (map (lambda (p) (list (car p) '(quote *unassigned*))) pairs)
                     (concat (map (lambda (p) (list 'set! (car p) (cadr p))) pairs)
                             body)))))

(define (define-variable! var val env)
  (let* ((frame (current env))
         (vals (lookup-frame var frame)))
    (if vals
        (set-car! vals val)
        (set-car! env (make-frame (cons var (variables frame))
                                  (cons val (values frame)))))))

(define (lookup-env var env)
  (if (null? env)
      false
      (or (lookup-frame var (current env))
          (lookup-env var (enclosing env)))))

(define current car)
(define enclosing cdr)

(define (lookup-frame var frame)
  (let loop ((vars (variables frame))
             (vals (values frame)))
    (cond
     ((null? vars) false)
     ((eq? (car vars) var) vals)
     (else (loop (cdr vars) (cdr vals))))))

(define (definition-variable exp)
  (let ((var (cadr exp)))
    (if (variable? var)
        var
        (car var))))

(define (definition-value exp)
  (let ((var (cadr exp)))
    (if (variable? var)
        (caddr exp)
        (make-lambda (cdr var)
                     (cddr exp)))))

(define (make-lambda pairs body)
  (list-cons 'lambda pairs body))

(define (list-cons x y . zs)
  (let ((l (reverse (cons x (cons y zs)))))
    (let loop ((ret (car l))
               (l (cdr l)))
      (if (null? l)
          ret
          (loop (cons (car l) ret) (cdr l))))))

(define (set-variable-value! var val env)
  (let ((vals (lookup-env var env)))
    (if vals
        (set-car! vals val)
        (error (str "Unbound variable -- set!: " var)))))

(define (lookup-variable-value var env)
  (let ((vals (lookup-env var env)))
    (if vals
        (let ((val (car vals)))
          (if (eq? val '*unassigned*)
              (error (str "Unassigned var -- lookup-variable-value: " var))
              val))
        (error (str "Unbound variable -- lookup-variable-value: " var)))))

(define (eval-definition var val env)
  (define-variable! var val env)
  var)

(define (eval-sequence exps env)
  (if (null? exps)
      (error (str "empty exps -- eval-sequence"))
      (let loop ((head (car exps))
                 (more (cdr exps)))
        (if (null? more)
            (_eval head env)
            (begin (_eval head env)
                   (loop (car more)
                         (cdr more)))))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error (str "Too many arguments supplied: " vars vals))
          (error (str "Too few arguments supplied: " vars vals)))))

(define primitive-implementation cadr)
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define procedure-parameters cadr)
(define procedure-body caddr)
(define procedure-environment cadddr)

(define lambda-parameters cadr)
(define lambda-body cddr)

(define if-predicate cadr)
(define if-consequent caddr)
(define (if-alternative exp)
  (let [alt (cdddr)]
    (if (null? alt)
        false
        (car alt))))

(define (list-of-values exps env) (map (lambda (e) (_eval e env)) exps))

(define text-of-quotation cadr)
(define set!-variable cadr)
(define set!-value caddr)
(define operator car)
(define operands cdr)

(define my-false? false?)
(define (my-true? x) (not (my-false? x)))
(define (not x) (if x false true))

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)))

(define variable? symbol?)

(define (quote? exp) (tagged-list? exp 'quote))
(define (set!? exp) (tagged-list? exp 'set!))
(define (define? exp) (tagged-list? exp 'define))
(define (if? exp) (tagged-list? exp 'if))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (begin? exp) (tagged-list? exp 'begin))
(define (cond? exp) (tagged-list? exp 'cond))
(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (procedure? exp) (tagged-list? exp 'procedure))
(define (primitive? exp) (tagged-list? exp 'primitive))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (= (car exp) tag)))
