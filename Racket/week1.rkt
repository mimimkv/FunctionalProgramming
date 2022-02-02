#lang racket
(define (min x y)(if (> x y) y
                      x))
;Да се дефинира функцията inside? x a b, която проверява дали числото x се намира в затворения интервал [a, b].
(define (inside? x a b)(if (and (<= x b) (>= x a)) "yes"
                           "no"))
(inside? 4 2 7)



;Да се напише функция myfunc, която пресмята средно аритметично на квадратите на 2 числа
(define (square x) (* x x))
(define (myfunc a b) (/ (+ (square a) (square b)) 2))
(myfunc 4 2)

;fact
(define (fact x) (if (< x 1) 1
                     (* x (fact(- x 1)))))
(fact 5)

