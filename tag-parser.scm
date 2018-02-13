(load "qq.scm")

;; Util methods ;;

(define *reserved-words*
	'(and begin cond define do else if lambda let let* letrec or quasiquote 
		unquote unquote-splicing quote set!))

(define *void* (void))

(define is-in-list?
	(lambda (e list)
		(if (null? list)
			#f
			(if (equal? e (car list))
				#t
				(is-in-list? e (cdr list))))))

(define parse-args
	(lambda (processed-args args-to-process)
		;(display args-to-process) (newline)
		(if (pair? args-to-process)
			(parse-args (append processed-args (list (car args-to-process))) (cdr args-to-process))
			(list processed-args args-to-process))))

(define begin-remover
    (lambda (lst)
;    	(display "begin-remover: ") (display lst) (newline)
        (if (null? lst)
            '()
            (if (and (list? (car lst)) (pair? (car lst)))
                (if (equal? (caar lst) 'begin)
                    (append (cdr (begin-remover (car lst))) (begin-remover (cdr lst)))
                    (if (or (equal? (caar lst) 'lambda) (equal? (caar lst) `if))
                        (append (list (car lst)) (begin-remover (cdr lst)))
                        (append (list (begin-remover (car lst))) (begin-remover (cdr lst)))))
                (append (list (car lst)) (begin-remover (cdr lst)))))))

(define begin-adder-if-needed
	(lambda (exp) 
		;(display "begin-adder-if-needed: ") (display exp) (newline)
		(if (is-in-list? (car exp) *reserved-words*)
			exp
			(if (and (list? exp) (< 1 (length exp)) 
					  (not (if2? exp))
					  (not (if3? exp))) 
						(append (list 'begin) exp)
						(if (or (equal? `begin (car exp)) (equal? `let (car exp)) (equal? `if (car exp)))
							exp
							(car exp))))))

(define all-are-different
	(lambda (l)
		(if (pair? l)
			(and (not (member (car l) (cdr l)))
				(all-are-different (cdr l)))
			#t)))

;; parsers ;;

(define constant?
	(lambda (e) 
		(or (null? e)
			(vector? e)
            (boolean? e)
            (char? e)
            (number? e)
            (string? e))))

(define quoted?
	(lambda (e) 
		(and (list? e) (quote? e))))

(define variables?
	(lambda (e) 
		(and
			(not (is-in-list? e *reserved-words*))
			(not (list? e)))))

(define if2? 
	(lambda (e)
		(and
			(list? e)
			(equal? (length e) 3)
			(equal? `if (car e)))))

(define parse-if2
	(lambda (exp)
		;(display "parsing if2") (display exp) (newline)
		`(if3 ,(parse (cadr exp)) ,(parse (caddr exp)) (const ,*void*))))

(define if3?
	(lambda (e) 
		(and
			(list? e)
			(equal? (length e) 4)
			(equal? `if (car e))
			)))

(define parse-if3
	(lambda (exp) 
		;(display "parsing if3: ") (display exp) (newline)
		`(if3 ,(parse (cadr exp)) ,(parse (caddr exp)) ,(parse (cadddr exp)))))

(define or?
	(lambda (e) (and (list? e) (equal? `or (car e)))))

(define parse-or-helper
	(lambda (e)
;		(display "parsing or helper: ") (display e) (newline)
		(if (null? e)
			'()
			(append (list (parse (car e))) (parse-or-helper (cdr e))))))

(define parse-or
	(lambda (e)
		(cond 
			((= (length e) 1) (parse #f))
			((= (length e) 2) (parse (cadr e)))
			(#t (list 'or (parse-or-helper (cdr e)))))))

(define application? 
	(lambda (e) 
		(and 
			(list? e)
			(not (null? e))
			(not (is-in-list? (car e) *reserved-words*)))))

(define parse-application-helper
	(lambda (e)
		(if (null? e) 
			(list (list))
			(append (list (parse (car e))) (parse-application-helper (cdr e))))))

(define parse-application
	(lambda (e)
		;(display "parsing application: ") (display e) (newline)
		(append 
			(list 'applic) 
			(append (list (parse (car e)) (map parse (cdr e)))))))

(define lambda-varidaic?
	(lambda (e)
		(and 
			(list? e)
			(equal? `lambda (car e))
			(>= (length e) 3)
			(not (list? (cadr e)))
			(not (pair? (cadr e)))
			)))

(define lambda-variadic-parser
	(lambda (e)
		;(display "parsing lambda-variadic: ") (display e) (newline)
		(list 'lambda-opt '() (cadr e) (parse (begin-adder-if-needed (cddr e))))))

(define lambda-opt?
	(lambda (e)
		(and
			(list? e)
			(>= (length e) 3)
			(equal? `lambda (car e))
			(not (list? (cadr e)))
			(not (null? (caddr e))))))

(define lambda-opt-parser
	(lambda (e)
		;(display "parsing lambda-opt: ") (display e) (newline)
		(append (list 'lambda-opt) (parse-args '() (cadr e)) 
			(list (parse (begin-adder-if-needed (cddr e)))))))

(define lambda-simple?
	(lambda (e)
		(and 
			(list? e)
			(>= (length e) 3)
			(equal? `lambda (car e))
			(list? (cadr e))
			(not (null? (caddr e))))))

(define lambda-simple-parser
	(lambda (e)
		;(display "parsing lambda-simple: ") (display e) (newline)
		(list 'lambda-simple (cadr e) (parse (begin-adder-if-needed (cddr e))))))

(define define?
	(lambda (e)
		(and 
			(list? e)
			(equal? (length e) 3)
			(equal? `define (car e))
			(not (pair? (cadr e))))))

(define define-parser
	(lambda (e)
		;(display "parsing define: ") (display e) (newline)
		`(define (var ,(cadr e)) ,(parse (caddr e)))))

(define mit-define?
	(lambda (e)
		(and
			(list? e)
			(>= (length e) 3)
			(equal? `define (car e))
			(pair? (cadr e)))))

(define mit-define-parser
	(lambda (e)
		;(display "parsing mit-define: ") (display e) (newline)
		`(define (var ,(caadr e)) ,(parse (list 'lambda (cdadr e) (caddr e))))))

(define assignment?
	(lambda (e)
		(and
			(list? e)
			(= 3 (length e))
			(equal? `set! (car e))
			(variables? (cadr e)))))

(define assignment-parser
	(lambda (e)
		;(display "parsing assignment: ") (display e) (newline)
		`(set (var ,(cadr e)) ,(parse (caddr e)))))

(define sequence?
	(lambda (e)
		(and
			(list? e)
			(or 
				(equal? `begin (car e))
				))))

(define parse-sequence
	(lambda (e)
		;(display "parsing sequence: ") (display e) (newline)
		(cond 
			((> (length e) 2) (list 'seq (map parse (cdr (begin-remover e)))))
			((= (length e) 2) (parse (cadr e)))
			((= (length e) 1) `(const ,*void*)))))


;;;; Let cases ;;;;

(define get-var-or-arg-from-arg-list
	(lambda (operator arg-list)
		(if (pair? arg-list)
			(append (list (operator arg-list)) (get-var-or-arg-from-arg-list operator (cdr arg-list)))
			'())))

(define get-let-var-list
	(lambda (e)
		(get-var-or-arg-from-arg-list caar (cadr e))))

(define get-let-arg-list
	(lambda (e)
		(get-var-or-arg-from-arg-list cadar (cadr e))))

(define let?
	(lambda (e)
		(and
			(list? e)
			(equal? `let (car e))
			(list? (cadr e))
			(if (all-are-different (get-let-var-list e))
				#t
				(begin (error 'let "all variables must be different") #f)))))

(define parse-let
	(lambda (e)
		;(display "parsing let: ") (display e) (newline)
		(parse 
			(append 
				(list (append (list 'lambda) (list (get-let-var-list e)) 
										(list (begin-adder-if-needed (cddr e)))))
				(get-let-arg-list e))
		)))

(define let*?
	(lambda (e)
		(and
			(list? e)
			(equal? `let* (car e))
			(list? (cadr e)))))

(define let*-to-let-converter
	(lambda (e)
		;(display "converting let* to let: ") (display exp) (newline)
		(let ((var-arg-list (cadr e))
			  (exp (cddr e)))
			(cond
				((= 0 (length var-arg-list)) (append (list `let)  (list (list)) exp))
				((= 1 (length var-arg-list))
					(append (list `let) (list (list (car var-arg-list))) exp))
				(#t 
					(list `let (list (car var-arg-list)) 
					(let*-to-let-converter (append (list 'let*) (list (cdr var-arg-list)) exp))))))))

(define parse-let*
	(lambda (e)
		(parse (let*-to-let-converter e))))

(define letrec?
	(lambda (e)
		(and
			(list? e)
			(equal? `letrec (car e))
			(list? (cadr e))
			(if (all-are-different (get-let-var-list e))
				#t
				(begin (error 'letrec "all variables must be different") #f)))))

(define parse-letrec
	(lambda (e)
		;(display "parsing letrec: ") (display e) (newline)
		(let ((var-list (get-let-var-list e))
			  (arg-list (get-let-arg-list e))
			  (exps (cddr e)))
			(parse (append (list `let) 
					(list (map (lambda (var) (list var #f)) var-list))
					(map (lambda (var arg) (list `set! var arg)) var-list arg-list)
					(list (append (list `let (list)) exps)))))))

(define and?
	(lambda (e) (and (list? e) (equal? (car e) `and))))

(define convert-and-to-if
	(lambda (e)
		(if (> 4 (length e))
			e
			(list (car e) (cadr e) (convert-and-to-if (append (list `and) (cddr e)))))))

(define parse-and
	(lambda (e)
		;(display "parsing and: ") (display e) (newline)
		(cond ((= 1 (length e)) (parse #t))
		      ((= 2 (length e)) (parse (cadr e)))
		  	  ((= 3 (length e)) (parse (list `if (cadr e) (caddr e) #f)))
		  	  (#t (parse (convert-and-to-if e))))))

(define cond?
	(lambda (e)
		(and 
			(list? e)
			(< 1 (length e))
			(equal? `cond (car e)))))

(define convert-cond-to-if
	(lambda (e)
		;(display "converting to if: ") (display e) (newline)
		(if (equal? (caaddr e) `else)
			`(if ,(caadr e) ,(append (list 'begin) (cdadr e)) ,(append (list 'begin) (cdaddr e)))
			(list 'if (caadr e) (append (list 'begin) (cdadr e)) (convert-cond-to-if (append (list `cond) (cddr e)))))))

(define parse-cond
	(lambda (e)
		;(display "parsing cond: ") (display e) (newline)
		(cond ((= 2 (length e)) (parse `(if ,(caadr e) ,(append (list 'begin) (cdadr e)))))
		      ((equal? `else (caaddr e)) (parse `(if ,(caadr e) ,(append (list 'begin) (cdadr e)) ,(append (list 'begin) (cdaddr e)))))
		  	  (#t (parse (convert-cond-to-if e))))))

(define quasiquote?
	(lambda (e)
		(and
			(list? e)
			(equal? (string->symbol "quasiquote") (car e)))))

(define parse-quasiquote
	(lambda (e)
		;(display "qq: ") (display e) (newline)
		(parse (expand-qq (cadr e)))))

(define parse
	(lambda (e)
		;(display "parsing: ") (display e) (newline)
		(cond 
			((constant? e) `(const ,e))
			((quoted? e) `(const ,(cadr e)))
			((if2? e) (parse-if2 e))
			((if3? e) (parse-if3 e))
			((or? e) (parse-or e))
			((lambda-varidaic? e) (lambda-variadic-parser e))
			((lambda-opt? e) (lambda-opt-parser e))
			((lambda-simple? e) (lambda-simple-parser e))
			((define? e) (define-parser e))
			((mit-define? e) (mit-define-parser e))
			((assignment? e) (assignment-parser e))
			((let? e) (parse-let e))
			((let*? e) (parse-let* e))
			((letrec? e) (parse-letrec e))
			((and? e) (parse-and e))
			((cond? e) (parse-cond e))
			((quasiquote? e) (parse-quasiquote e))
			((sequence? e) (parse-sequence e))
			((application? e) (parse-application e))
			((variables? e) `(var ,e))
			(#t
				(error 'parse (format "The expression ~s is not parsable\n" e)))
		)
	))