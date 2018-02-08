(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")
(load "code-gen.scm")
;;;;;;;;;;;;;;;;;; Util methods ;;;;;;;;;;;;;;;;

(define remove-duplicates
	(lambda (list)
		(if (or (not (list? list)) (null? list))
			'()
			(cons
				(car list) 
				(remove-duplicates 
					(filter 
						(lambda (x) (not (equal? (car list) x))) 
						(cdr list)))))))

(define flatten-once-max 
	(lambda (x)
		(flatten-helper '() x)))

(define flatten-helper
	(lambda (flat to-flatten)
		(if (null? to-flatten)
			flat
			(let ((current (car to-flatten)))
				(cond 
		  			((and (list? current) (not (null? current))) (flatten-helper (append flat current) (cdr to-flatten)))
		  			(#t (flatten-helper (append flat (list current)) (cdr to-flatten))))))))

(define completely-flatten
	(lambda (list)
		(if (equal? list (flatten-once-max list))
			list
			(completely-flatten (flatten-once-max list)))))

(define find-in-const-list
	(lambda (type const const-list)
		(let ((const-record (car const-list)))
			(if (and (equal? type (cadr const-record)) (equal? const (caddr const-record)))
				(car const-record)
				(find-in-const-list type const (cdr const-list))))))

(define get-const-type
	(lambda (const)
		(cond 
			((eq? const (void)) 'T_VOID)
			((null? const) 'T_NIL)
			((integer? const) 'T_INTEGER)
			((number? const) 'T_FRACTION)
			((or (eq? #f const) (eq? #t const) 'T_BOOL)
			((char? const) 'T_CHAR)
			((string? const) 'T_STRING)
			((symbol? const) 'T_SYMBOL)
			((pair? const) 'T_PAIR)
			(#t 'undef)))))

(define is-last?
	(lambda (val list)
		(equal? val (car (reverse list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Read and Write to File ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define read-from-file
	(lambda (in-file)
		(let ((in-port (open-input-file in-file)))
			(letrec ((run
				(lambda ()
					(let ((ch (read-char in-port)))
						(if (eof-object? ch)
							(begin (close-input-port in-port) '())
							(cons ch (run)))))))
				(run)))))

(define write-to-file 
	(lambda (out-file str)
		(let ((out-port (open-output-file out-file 'replace)))
			(display str out-port)
			(close-output-port out-port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pipeline
	(lambda (s)
		((star <sexpr>) s
			(lambda (m r)
				(map (lambda (e)
					(annotate-tc
						(pe->lex-pe
							(box-set
								(remove-applic-lambda-nil
									(parse e))))))
				m))
			(lambda (f) 'fail))))

(define get-consts-from-sexpr
	(lambda (sexpr)
;		(display (format "Getting a const from: ~A, number? ~A\n" sexpr (number? sexpr)))
		(cond 
			((and (not (integer? sexpr)) (number? sexpr)) (list (numerator sexpr) (denominator sexpr) sexpr))
			((pair? sexpr) (extract-sub-consts-from-pair sexpr))
		    (#t sexpr))))

(define extract-sub-consts-from-pair
	(lambda (p)
		(let 
			((base-consts (completely-flatten p))
			(sub-pairs (make-sub-pairs p)))
			(append base-consts sub-pairs) p)))

(define make-sub-pairs
	(lambda (p)
		p))

(define find-consts
	(lambda (parse-list)
;		(display (format "parse-list: ~A\n" parse-list)) (newline)
		(flatten-once-max 
			(map
				(lambda (sexpr) 
					(cond
						((eq? 'const (car sexpr)) (get-consts-from-sexpr (cadr sexpr)))
					    ((list? parse-list) (find-consts sexpr))))
				(filter list? parse-list)))))

(define get-address
	(let ((address -1))
		(lambda ()
			(set! address (+ 1 address))
			address)))

(define create-const-list
	(lambda (const-list)
;		(display (format "Set of consts: ~A\n" const-list))
		(create-const-list-helper const-list)))

(define create-const-list-helper
	(lambda (const-list)
;		(display "const-list: ") (display const-list) (newline)
		(if (null? const-list)
			'()
			(let* 
				((const (car const-list))
				(const-label 
				(cond 
					((null? const) 
						(list (get-address) 'T_NIL 0))
					((eq? const (void))
						(list (get-address) 'T_VOID void))
    				((integer? const)
    					(list (get-address) 'T_INTEGER const))
    				((number? const)
						(list (get-address) 'T_FRACTION (numerator const) (denominator const)))
					((eq? #f const)
    					(list (get-address) 'T_BOOL 0))
    				((eq? #t const) 
    					(list (get-address) 'T_BOOL 1))
    				((char? const)
    					(list (get-address) 'T_CHAR (char->integer const)))
    				((string? const)
    					(list (get-address) 'T_STRING const))
    				((symbol? const)
    					(list (get-address) 'T_SYMBOL const))
    				((pair? const)
    					(list (get-address) 'T_PAIR (car const) (cdr const)))
					(#t 'undef))))
				(cons const-label (create-const-list-helper (cdr const-list)))))))

(define make-const-table
	(lambda (parsed-code)
		(display (format "Result of flatten: ~A\n" (find-consts parsed-code)))
		(create-const-list
			(remove-duplicates
				(append
					`(,(void) ,'() ,#f ,#t)
					(find-consts parsed-code))))))

(define convert-const-record-to-assembly 
	(lambda (const-record)
		(format 
"const_~A:
	dq MAKE_LITERAL(~A, ~A)
" (car const-record) (cadr const-record) (caddr const-record))))

(define convert-fraction-to-assembly
	(lambda (const-record const-list)
;		(display (format "Converting fraction ~A" const-record))
		(let 
			((numer (find-in-const-list 'T_INTEGER (caddr const-record) const-list))
			(denom (find-in-const-list 'T_INTEGER (cadddr const-record) const-list)))
		(format
"const_~A:
	dq MAKE_LITERAL_FRACTION(const_~A, const_~A)
" (car const-record) numer denom))))

(define convert-pair-to-assembly
	(lambda (const-record const-list)
;		(display (format "Converting pair ~A" const-record))
		(let 
			((place-of-car (find-in-const-list (get-const-type (caddr const-record)) (caddr const-record) const-list))
			(place-of-cdr (find-in-const-list (get-const-type (cadddr const-record)) (cadddr const-record) const-list)))
		(format
"const_~A:
	dq MAKE_LITERAL_PAIR(const_~A, const_~A)
" (car const-record) place-of-car place-of-cdr))))

(define find-vector-vals-places
	(lambda (vals-list const-list)
		(map
			(lambda (val)
				(cond 
					((integer? val) (find-in-const-list 'T_INTEGER val const-list))
					(#t (begin (display (format "the val ~A is not yet supported for assembly declaration" val)) #f))))
			vals-list)))

(define create-vector-creation-args
	(lambda (val-places)
		(fold-left
			string-append
			" "
			(map
				(lambda (place) 
					(if (is-last? place val-places)
						(format "const_~A" place)
						(format "const_~A, " place)))
				val-places))))

(define convert-vector-to-assembly
	(lambda (const-record const-list)
		(let ((vals-places (find-vector-vals-places (cddr const-record) const-list)))
			(string-append
				(format "const_~A:\n    dq MAKE_LITERAL_VECTOR" (car const-record))
				(create-vector-creation-args vals-places)
				"\n"))))

(define extract-const-table
	(lambda (const-table)
		(string-append
			"section .data\nstart_of_data:\n" 
			(fold-right
				string-append
				""
				(map
					(lambda (const-record) 
						(let ((type (cadr const-record)))
							(cond 
								((equal? 'T_NIL type) (format "const_~A:\n    dq SOB_NIL\n" (car const-record))) 
								((or (equal? 'T_VOID type) (equal? 'T_NIL type) 
									(equal? type 'T_CHAR) (equal? type 'T_INTEGER) (equal? 'T_BOOL type))
									(convert-const-record-to-assembly const-record))
							    ((equal? 'T_FRACTION type) (convert-fraction-to-assembly const-record const-table))
							    ((equal? 'T_PAIR type) (convert-pair-to-assembly const-record const-table))
							    ((equal? 'T_VECTOR type) (convert-vector-to-assembly const-record const-table))
								(#t 
									(display (format "extract-cons-table: The const type ~A is not yet supported for creation\n" type))))))
					const-table))
			"\n")))

(define prolog
	"%include \"scheme.s\"

")

(define epilog
"END:
    ret
")

(define code-header "extern exit, printf, scanf
global main, write_sob, write_sob_if_not_void
section .text
main:
")

(define compile-scheme-file
	(lambda (in-file out-file)
		(let* 
			((parsed-scheme-code (pipeline (read-from-file in-file)))
			(constants-table (make-const-table parsed-scheme-code))
			(symbol-table (filter (lambda (x) #t) constants-table))
			(global-variable-table 'undef))
			(display (format "constants-table: ~A" constants-table)) (newline)
			(write-to-file out-file
				(string-append 
					prolog 
					(extract-const-table constants-table)
					(code-gen parsed-scheme-code constants-table)
					epilog
					)))))
