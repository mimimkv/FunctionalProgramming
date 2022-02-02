#lang racket


;Задача 1. Да се дефинира функция, която намира дължината на списък.

(define (size lst)
  (if (null? lst)
      0
      (+ 1 (size (cdr lst)))))

(size '( 1 2 3))
(size (cons 1 (cons 2 '())))
(size (list 1 2 3))

;Задача 2. Да се дефинира функция, която проверява дали даден елемент се
;съдържа в списък.
(define (member? x lst)
  (cond [(null? lst)   #f]
        [(= x (car lst))   #t]
        [else            (member? x (cdr lst))]))
(member? 2 '(1 5 -3 0))
(member? 6 '(2 3 4 6 7))
        

;Задача 3. Да се дефинира функция, която добавя елемент на зададена
;позиция в списък.
(define (insert-at lst el pos)
  (if (or (= pos 0) (null? lst))
      (cons el lst)
      (cons (car lst) (insert-at (cdr lst) el (- pos 1)))))

(insert-at '(1 2 3 4 5) 15 10)
;Задача 4. Да се дефинира функция, която намира най-малкия елемент на списък.
(define (min-lst lst)
  (if (null? (cdr lst)) 
      (car lst)
      (min (car lst) (min-lst (cdr lst)))))
(min-lst '(10 2 3 4))

;Задача 5. Да се дефинира функция, която изтрива първото срещане на даден
;елемент в списък.

(define (erase-first el lst)
  (cond [(null? lst)  '()]
        [(= (car lst) el) (cdr lst)]
        [else             (cons (car lst)
                                (erase-first el (cdr lst)))]))
(erase-first 5 '(1 2 4))
(erase-first 5 '(1 2 5 4 5))

;Задача 6. Да се дефинира функция, която изтрива всички срещания на даден
;елемент на списък.
(define (erase-all el lst)
  (cond [(null? lst) '()]
        [(= (car lst) el) (erase-all el (cdr lst))]
        [else             (cons (car lst)
                                (erase-all el (cdr lst)))]))
(erase-all 5 '(1 2 4))
(erase-all 5 '(1 2 5 4 5))

;Задача 7. Да се напише функция, която конкатенира два списъка.
; (concat '(1 2 3) '(4 5 6)) -> '(1 2 3 4 5 6)
; (cons 1 (cons 2 (cons 3 '(4 5 6))))

(define (concat lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (concat (cdr lst1) lst2))))
(concat '(1 2 3) '())
(concat '(1 2 3) '(4 5 6))

;Задача 8. Да се напише функция, която обръща даден списък.
; (reverse '(1 2 3)) -> '(3 2 1)
; (cons 3 (cons 2 (cons 1 '())))

(define (reverse lst)
  (define (helper lst rev-lst)
    (if (null? lst)
        rev-lst
        (helper (cdr lst) (cons (car lst) rev-lst))))
  (helper lst '()))
(reverse '(1 2 3))
