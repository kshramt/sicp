(print (= (_eval '(begin (define (f a b) (+ a b)) (f 1 2)) (setup-environment)) 3) "\n")
(print (= (_eval '(begin (define (f a b) (- a b) (+ a b)) (f 1 2)) (setup-environment)) 3) "\n")
;; (begin (driver-loop (setup-environment) (current-input-port))
;;        'bey)
