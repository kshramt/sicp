(include "scheme1.scm")

(print
 (= (_eval '(begin (define (f a b) (+ a b)) (f 1 2)) (setup-environment)) 3) "\n"
 (= (_eval '(begin (define (f a b) (- a b) (+ a b)) (f 1 2)) (setup-environment)) 3) "\n"
 (= (_eval '(begin (define (f x) (define (g x) x) (g x)) (f 1)) (setup-environment)) 1) "\n"
 (= (_eval '(begin (define (f x) (define (g x) x) (g x)) (f 1)) (setup-environment)) 1) "\n"
 (= (_eval '(begin (define (factorial n)
                     (define (impl k ret)
                       (if (= k 1)
                           ret
                           (impl (- k 1) (* k ret))))
                     (impl n 1))
                   (factorial 7))
           (setup-environment))
    5040)
 "\n"
 (= (_eval '(letrec ((fact
                      (lambda (n)
                        (if (= n 1)
                            1
                            (* n (fact (- n 1)))))))
              (fact 5))
           (setup-environment))
    120)
 "\n"
 (= (_eval '((lambda (n)
               ((lambda (fib)
                  (fib fib n))
                (lambda (fb n)
                  (if (< n 2)
                      1
                      (+ (fb fb (- n 1))
                         (fb fb (- n 2)))))))
             7)
           (setup-environment))
    21)
 "\n"
 )
;; (begin (driver-loop (setup-environment) (current-input-port))
;;        'bey)
