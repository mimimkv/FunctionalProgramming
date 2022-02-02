#lang racket


;Задача 5. Функция (maxOrderedSublist xs), която връща най-дългия
;възходящо сортиран подсписък от списъка xs.
(define (max-ordered-prefix xs)
  (cond [(null? xs)     '()]
        [(or (null? (cdr xs))
             (>= (car xs) (cadr xs))) (list (car xs))]
        [else (cons       (car xs) (max-ordered-prefix (cdr xs)))]))

(define (max-ordered-sublist xs)
  (define (helper xs max-subxs)
    (define cur-ord-pre (max-ordered-prefix xs));
    (cond [(null? xs) max-subxs]
          [(> (length cur-ord-pre) (length max-subxs))
              (helper (drop xs (length cur-ord-pre)) cur-ord-pre)]
          [else (helper (drop xs (length cur-ord-pre)) max-subxs)]))
  (helper xs '()))

(max-ordered-sublist '(1 2 3 1 2 3 4 5 1 2))
(max-ordered-sublist '(1 2 3))
(max-ordered-sublist '(5 1))



              
;Задача 6. Функция (flatten xss), която приема списък от списъци
;(които също могат да са от списъци, т.е. имаме произволно ниво на вложение)
;и връша списък само от елементи, т.е. списък без вложени списъци

;Пример:  -> '(1 2 3 4 5 6 7 8 9 10 11 12)
(define (flatten xs)
  (cond [(null? xs)       '()]
        [(list? (car xs)) (flatten (append (car xs) (cdr xs)))]
        [else             (cons (car xs) (flatten (cdr xs)))]))

(flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))
(flatten '('(1 2 3) '(4) 5 + -))


;Задача 1. "Метрика" наричаме функция, която приема като параметър списък от числа и връща число като резултат. Да се напише функция best-metric?,
;която приема като параметри списък от метрики ms и списък от списъци от числа xss и проверява дали има метрика в ms, която дава
;по-големи стойности от всички други метрики от ms над всеки от елементите на xs.
;Пример:
;(define (prod xs) (apply * xs))
;(define (sum xs) (apply + xs))
;(best-metric? (list sum prod) `((0 1 2) (3 -4 5) (1337 0)))  ; -> #t
;(best-metric? (list car sum) `((100 -100) (29 1) (42)))      ; -> #f

;(best-metric? (list car prod sum) '((-100 1) (29 1) (42)))
;-100 29 42
;-100 29 42
;-99 30 42


(define (prod1 xs) (apply * xs))
(define (sum1 xs) (apply + xs))
(define (f1 ms xss)
  (map (λ (m) (map m xss)) ms))
(f1 (list car prod1 sum1) '((-100 1) (29 1) (42)))

(define (best-metric? ms xss)
  (define applied-metrics (map (λ (m) (map m xss)) ms))
  (define (helper aps)
    (cond [(null? aps) #f]
          [(andmap (λ (m) (andmap >= (car aps) m)) applied-metrics)
             #t]
          [else (helper (cdr aps))]))
  (helper applied-metrics))


  
;(andmap > '(3 4 1337) '(0 -60 0))
; Пример:
(define (prod xs) (apply * xs))
(define (sum xs) (apply + xs))

(best-metric? (list sum prod) `((0 1 2) (3 -4 5) (1337 0)))  ; -> #t
(best-metric? (list car sum) `((100 -100) (29 1) (42)))      ; -> #f
