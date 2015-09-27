(print
 (= (_eval '(begin (define (f a b) (+ a b)) (f 1 2)) (setup-environment)) 3) "\n"
 (= (_eval '(begin (define (f a b) (- a b) (+ a b)) (f 1 2)) (setup-environment)) 3) "\n"
 (= (_eval '(begin (define (f x) (define (g x) x) (g x)) (f 1)) (setup-environment)) 1) "\n"
 (= (_eval '(begin (define (f x) (define (g x) x) (g x)) (f 1)) (setup-environment)) 1) "\n"
 (= (_eval '((lambda (n)
               ((lambda (fib)
                  (fib fib n))
                (lambda (fb n)
                  (if (< n 2)
                      1
                      (+ (fb fb (- n 1))
                         (fb fb (- n 2)))))))
             6)
           (setup-environment))
    13) "\n"
 )
;; (begin (driver-loop (setup-environment) (current-input-port))
;;        'bey)
