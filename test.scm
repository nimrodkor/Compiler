;(define y (lambda (x) x))
;(define x 123)
;y
;(y 3)
;(if #f x y)
;2
;5
;10
;1276
;#f
;#t
;'()
;#\a
;#\z
;3/5
;'(1 2 (3))
;'(10 . 2)
;'(10 20 30)
;#(32 42 66 78 100 120 2)
;'(1 (2) (3 4))
;'(1 (2 . 3))
;#(1 2 3 #(4 5 6))
;#(1 (2 3 4) 3 (20 . 39) 4)
;'(1 . #(1 2 3))
;'(1 #(1 2 3))
;(begin '(2 . 3) #(60 70 80))
;'("hi" . 2)
;"Nimrod"
;#("hi" "there" "Nati!")
;(lambda (y z) (lambda (a) 66))
;(begin 3 4 5)
;((lambda (x y) 2) 3 4)
;((lambda (x) "Nati") 3)
;((lambda () "Nimrod"))

;((lambda () ((lambda (x) (set! x 4) x) '(2 16))))
;(lambda x x)
;((lambda x 2))
;((lambda x x) "Shati")
;(or 1 2 3)
;(or #f #t)
;(or)

;(if #t "True" "False")
;(if #f "True" "False")
;((lambda (x) x) #f)
;not
;(not #f)
;(not 3)
;car
;cdr
;(car '(1 . 2))
;(cdr '(1 . 2))
;(char? #\a)
;(char? 1)
;(not (char? 1))
;(integer? 2)
;(integer? 1/3)
;(number? 1/3)
;(number? #\a)
;'(1 . 2)
;(pair? '(1 . 2))
;(pair? #f)
;(procedure? not)
;(define pair-example '(1 . 2))
;pair-example
;set-car!
;(set-car! pair-example 2)
;pair-example
;set-cdr!
;(set-cdr! pair-example 5)
;pair-example
;(lambda x x)
;=
;(= 2)
;(= 2 3)
;(= 2 2)
(= 2 2 2 2)
;(= 2 2 2 2 3)
>
(> 2)
(> 2 3)
(> 3 1)
(> 6 5 4 3 2 1)
(> 6 5 4 1 2 1)
<
(< 2 3)
(< 3 1)
(< 1 2 3 4 5 6)
(< 1 2 3 4 6 5)
apply
(apply = '(2 2 2))
