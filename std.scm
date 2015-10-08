(define (assert . x)
  (let ((n (length x)))
    (cond
     ((= n 1) (or (car x) (error "Assertion failure")))
     ((= n 2) (or (car x) (error (cadr x))))
     (else (error (str "Arity error -- assert: " n))))))
(define (test x)
  (print ".")
  (assert (true? x) "Test failure"))

(define (print . x) (__print x))
(define (str . x) (__str x))

(define (<= x y) (or (< x y) (= x y)))
(define (>= x y) (or (> x y) (= x y)))

(define (length p)
  (let loop ((p p)
             (n 0))
    (if (null? p)
        n
        (loop (cdr p) (+ n 1)))))

(define (concat . xss)
  (define (concat2 x y)
    (foldr-tc cons x y))
  (foldr-tc concat2 xss '()))

(define (foldl f init xs)
  (if (null? xs)
      init
      (foldl f (f init (car xs)) (cdr xs))))

(define (trans/foldl xf rf init xs)
  (foldl (xf rf) init xs))

(define (foldr f xs init)
  (if (null? xs)
      init
      (f (car xs) (foldr (cdr xs) init))))

(define (foldr-tc f xs init)
  (foldl (flip f) init (reverse xs)))

(define (flip f) (lambda (x y) (f y x)))

(define (map f xs) (reverse (reverse-map f xs)))
(define (reverse-map f xs)
  (let loop ((xs xs)
             (ret '()))
    (if (null? xs)
        ret
        (loop (cdr xs)
              (cons (f (car xs)) ret)))))
(define (trans/map f)
  (lambda (rf)
    (lambda (init x)
      (rf init (f x)))))

(define (reverse xs)
  (let loop ((xs xs)
             (ret '()))
    (if (null? xs)
        ret
        (loop (cdr xs) (cons (car xs) ret)))))

(define (caar p) (car (car p)))
(define (cadr p) (car (cdr p)))
(define (cdar p) (cdr (car p)))
(define (cddr p) (cdr (cdr p)))
(define (caaar p) (car (car (car p))))
(define (caadr p) (car (car (cdr p))))
(define (cadar p) (car (cdr (car p))))
(define (caddr p) (car (cdr (cdr p))))
(define (cdaar p) (cdr (car (car p))))
(define (cdadr p) (cdr (car (cdr p))))
(define (cddar p) (cdr (cdr (car p))))
(define (cdddr p) (cdr (cdr (cdr p))))
(define (caaaar p) (car (car (car (car p)))))
(define (caaadr p) (car (car (car (cdr p)))))
(define (caadar p) (car (car (cdr (car p)))))
(define (caaddr p) (car (car (cdr (cdr p)))))
(define (cadaar p) (car (cdr (car (car p)))))
(define (cadadr p) (car (cdr (car (cdr p)))))
(define (caddar p) (car (cdr (cdr (car p)))))
(define (cadddr p) (car (cdr (cdr (cdr p)))))
(define (cdaaar p) (cdr (car (car (car p)))))
(define (cdaadr p) (cdr (car (car (cdr p)))))
(define (cdadar p) (cdr (car (cdr (car p)))))
(define (cdaddr p) (cdr (car (cdr (cdr p)))))
(define (cddaar p) (cdr (cdr (car (car p)))))
(define (cddadr p) (cdr (cdr (car (cdr p)))))
(define (cdddar p) (cdr (cdr (cdr (car p)))))
(define (cddddr p) (cdr (cdr (cdr (cdr p)))))
