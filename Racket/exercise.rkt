#lang racket


;task1

(define (decreasing? n)
  (define (helper num last)
    (cond [(< num 10)   #t]
          [(> last (remainder (quotient num 10) 10)) #f]
          [else (helper (quotient num 10) (remainder (quotient num 10) 10))]))
  (helper n (remainder n 10)))
(decreasing? 1234)
(decreasing? 1111)
(decreasing? 444321)
(decreasing? 199)
(decreasing? 200)

(define (sum-numbers a b)
  (cond [(> a b) 0]
        [(decreasing? a) (+ a (sum-numbers (+ a 1) b))]
        [else (sum-numbers (+ a 1) b)]))
(sum-numbers 1 9) ;→ 45
(sum-numbers 199 203); → 200
(sum-numbers 219 225); → 663


;task2


(define (bigger-count element lst)
  (cond [(null? lst) 0]
        [(< element (car lst)) (+ 1 (bigger-count element (cdr lst)))]
        [else (bigger-count element (cdr lst))]))



(define (num-bigger-elements lst)
   (define (helper result temp)
      (cond [(null? temp) (list result)]
            [else (cons result
                  (helper (list (car temp) (bigger-count (car temp) lst)) (cdr temp)))]))
  (helper (list (car lst) (bigger-count (car lst) lst)) (cdr lst)))

(num-bigger-elements '(5 6 3 4)) ;→ '((5 1) (6 0) (3 3) (4 2))
(num-bigger-elements '(1 1 1)); → '((1 0) (1 0) (1 0))


(define (num-bigger-elements2 lst)
    (map (λ (x) (list x (bigger-count x lst))) lst)) 
(num-bigger-elements2 '(5 6 3 4)) ;→ '((5 1) (6 0) (3 3) (4 2))
(num-bigger-elements2 '(1 1 1)); → '((1 0) (1 0) (1 0))


;task3
;Задача 3. Ако f и g са числови функции и n е естествено число, да се дефинира
;функция от повисок ред (switchsum f g n), която връща като резултат функция,
;чиято стойност в дадена
;точка x е равна на f(x)+g(f(x))+f(g(f(x)))+ ... (сумата включва n събираеми).

(define (switchsum f g n)
  (define (helper count prev result)
    (cond [(>= count n) result] ; >, rem = 1
          [(= (remainder count 2) 0) (helper (+ count 1)
                                             (g prev)
                                             (+ result prev))]
          [else (helper (+ count 1) (f prev) (+ result prev))]))
  (λ (x) (helper 0 (f x) 0)))
          

((switchsum (lambda (x) (+ x 1))
 (lambda (x) (* x 2)) 1) 2) ;→ 3        1 is n, 2 is x
((switchsum (lambda (x) (+ x 1))
 (lambda (x) (* x 2)) 2) 2) ;→ 9
((switchsum (lambda (x) (+ x 1))
 (lambda (x) (* x 2)) 3) 2); → 16
((switchsum (lambda (x) (+ x 1))
 (lambda (x) (* x 2)) 4) 2) ;→ 30



;task4
;Да се дефинира функция (repeater str), която получава като аргумент символен
;низ и връща анонимна функция на два аргумента - count и glue (число и низ).
;Оценката на обръщението към върнатата функция е низ, който се получава чрез
;count-кратно повтаряне
;> ((repeater "I love Racket") 3 " ")
;"I love Racket I love Racket I love Racket"
;> ((repeater "Quack") 5 "!")
;"Quack!Quack!Quack!Quack!Quack"

;на низа str, при което между всеки две съседни повторения на str стои низът glue.
;Помощна информация. За да съедините няколко низа, може да използвате вградената
;функцията string-append:
;> (string-append "I" "Love" "Racket")
;"ILoveRacket"
;Функцията string-append приема произволен брой агрументи и връща низ, който
;представлява тяхната конкатенация

(define (repeater2 str)
  (define (helper count glue)
    (if (= count 1)
        str
        (string-append (helper (- count 1) glue) glue str)))
  helper)

((repeater2 "abcd") 3 " ")



(define (repeater str)
  (define (rep count glue res)
    (cond [(= count 0) res]
          [else (rep (- count 1)
                     glue
                     (string-append res glue str))]))
  (λ (count glue) (if (= count 0)
                      ""
                      (rep (- count 1) glue str))))
((repeater "I love Racket") 3 " ")

;task5
;Задача 5. Да се дефинира функция (sum-sum-digit a b k), която намира сумата на
;естествените числа от a до b (0<a≤b), сумата от цифрите на които е кратна на k
;

(define (sum-digits n)
  (if (=  n 0)
      0
      (+ (remainder n 10) (sum-digits (quotient n 10)))))
(sum-digits 1332409)
(sum-digits 20)

(define (sum-sum-digit a b k)
  (define (helper result i)
    (cond [(> i b) result]
          [(= (remainder (sum-digits i) k) 0)
           (helper (+ result i) (+ i 1))]
          [else (helper result (+ i 1))]))
  (helper 0 a))

(sum-sum-digit 1 10 3)
(sum-sum-digit 3 20 4) ; 4 + 8 + 13 + 17 = 42


;task7
;Да се дефинира функция (where list-elements list-predicates), която
;връща списък от всички елементи на list-elements, за които са изпълнени всички
;предикати в list-predicates.

(define (where list-elements list-predicates)
  (if (or (null? list-predicates) (null? list-elements))
      list-elements
      (where (filter (car list-predicates) list-elements)
                     (cdr list-predicates))))


(where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) ;→
;(6 8 10) (списък от всички елементи на дадения, които са четни числа, по-големи от 5)
(where '(3 4 5 7) (list even? (lambda (x) (> x 5)))); → () (в списъка
;няма четни числа, по-големи от 5)



;task8
;Да се дефинира функция (set-union xs ys), която връща обединението на
;множествата от числа xs и ys, представени като списъци, наредени във възходящ ред.
;Елементите на резултантното множество също трябва да са наредени във възходящ ред.
;Примери:
;(set-union '(1 3 5 7) '(5 7 13)) → '(1 3 5 7 13)
;(set-union '(5 7 13) '(1 3 5 7)) → '(1 3 5 7 13)


(define (contains el lst)
  (cond [(null? lst) #f]
        [(= (car lst) el) #t]
        [else (contains el (cdr lst))]))

(define (set-union xs ys)
  (define (helper result xs ys)
    (cond [(and (null? xs) (null? ys)) (list result)]
          [(null? xs) (cons result ys)]
          [(null? ys) (cons result xs)]
          [(= (car xs) (car ys)) (cons result (helper (car xs) (cdr xs) (cdr ys)))]
          [(< (car xs) (car ys)) (cons result (helper (car xs) (cdr xs) ys))]
          [else                  (cons result (helper (car ys) xs (cdr ys)))]))
  (helper '() xs ys))

(set-union '(1 3 5 7) '(5 7 13)) ;→ '(1 3 5 7 13)
(set-union '(5 7 13) '(1 3 5 7)) ;→ '(1 3 5 7 13)



(define (set-union1 xs ys)
  (define (helper result xs ys)
    (cond [(and (null? xs) (null? ys)) result]
          [(null? xs) (append ys result)]
          [(null? ys) (append xs result )]
          [(= (car xs) (car ys)) (helper (cons (car xs) result) (cdr xs) (cdr ys))]
          [(< (car xs) (car ys)) (helper (cons (car xs) result ) (cdr xs) ys)]
          [else                  (helper (cons (car ys) result) xs (cdr ys))]))
  (reverse (helper '() xs ys)))

(set-union1 '(1 3 5 7) '(5 7 13)) ;→ '(1 3 5 7 13)
(set-union1 '(5 7 13) '(1 3 5 7)) ;→ '(1 3 5 7 13)


