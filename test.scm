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
;(define pair-example '(10 . 20))
;pair-example
;set-car!
;(set-car! pair-example 2)
;pair-example
;set-cdr!
;(set-cdr! pair-example 16)
;pair-example
;(lambda x x)
;=
;(= 2)
;(= 2 3)
;(= 2 2)
;(= 2 2 2 2)
;(= 2 2 2 2 3)
;>
;(> 2)
;(> 2 3)
;(> 3 1)
;(> 6 5 4 3 2 1)
;(> 6 5 4 1 2 1)
;<
;(< 2 3)
;(< 3 1)
;(< 1 2 3 4 5 6)
;(< 1 2 3 4 6 5)
;(((lambda (x) (lambda (y) x)) 1) 2)
;((lambda x x) 2 3)
;(numerator 2)
;(numerator 2/3)
;(denominator 2)
;(denominator 2/3)
;+
;(+)
;(+ 1/2)
;(+ 2 3 4)
;((lambda (x) (+ x x)) 2)
;(+ 1/2 2/4)
;(+ 1/2 2)
;boolean?
;(boolean? #t)
;(boolean? #f)
;(boolean? 2)
;(= (+ 3 1) (+ 2 4/2))
;(*)
;(* 2 3)
;(* 1 2 3 4)
;(* 1/4 3 8)
;*
;/
;(/ 3 1/9)
;(/ 3)
;(/ 6 2 3)
;(/ 6 1/2 4/1)
;-
;(- 3)
;(- 16 2)
;(= (+ 2 2) (- 5 1) (* 4 1) (/ 24 2 3))
;(remainder 2 1)
;(remainder 4 3)
;char->integer
;(char->integer #\a)
;(integer->char (char->integer #\a))
;(cons 1 (cons 2 (cons 3 '())))
;list
;(list)
;(list 4 5 6)
;(list (+ 1 2))
;#(0 1 2 3)
;(make-vector 3)
;(make-vector 0)
;vector
;(vector 1 2 3)
;vector-length
;(vector-length (vector 1 2 3 4 5 6))
;vector-ref
;(vector 1 2 3 4 5 6)
;(define vec (vector 1 2 3 4 5 6))
;(vector-ref vec 3)
;(vector-ref (vector 1 2 3 4 5 6) 3)
;(vector-set! vec 3 10)
;(vector-ref vec 3)
;vec
;make-string
;(make-string 2)
;"123"
;(string-length "123")
;(define nim "Nimrod")
;(string-ref nim 5)
;(string-set! nim 0 #\n)
;nim
;(string-length "Nimrod")
;(append (list 1 2 3) (list 4 5 6))
;(zero? 0)
;(zero? (- 1 1))
;(zero? 3)
;(zero? #\a)
;(map zero? (list 0 3 0 2 (* 1 0) (/ 0 2) #\a))
;(map rational? (list 0 3 0 2 (* 1 0) (/ 0 2) #\b))
;'(1 . 2)
;(lambda (x) (lambda (y) x))
;((lambda (x) (lambda (y) x)) 3)
;(((lambda (x) (lambda (y) (+ x y))) 3) 4)
;'abc
;(symbol->string 'abc)
;(eq? 'abc 'abc)
;(define x 'a)
;(eq? x 'a)
;(define x 'b)
;(eq? x 'a)
;(string->symbol "a")
;(string->symbol "abc")
;(string->symbol "hello")
;(string->symbol "hello")
;((((lambda (x) (lambda (y) (set! x 2) (lambda (z) x))) 100) 200) 300)
;""
;(make-string 0)
;(/ 0 5 6 7)
;(/ 0 1/4 1/5)
;(/ 0 5000)
;(/ 0 -5000)
;(/ 1)
;(/ -1)
;(/ 5)
;(/ 4)
;(/ -4)
;(/ -2/5)
;(/ -1/2 -1/4)
;(/ -4/7)
;(/ -4/7 -4/7)
(/ 37/7 -23/7)
