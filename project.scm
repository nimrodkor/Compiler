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
			(if (not (list? to-flatten))
				(if (pair? to-flatten)
					(append flat (list (car to-flatten)) (list (cdr to-flatten)))
					(append flat (list to-flatten)))
				(let ((current (car to-flatten)))
					(cond 
			  			((and (list? current) (not (null? current))) (flatten-helper (append flat current) (cdr to-flatten)))
			  			((pair? current) (flatten-helper (append flat (flatten-helper '() (car current)) (flatten-helper '() (cdr current))) (cdr to-flatten)))
			  			(#t (flatten-helper (append flat (list current)) (cdr to-flatten)))))))))

(define completely-flatten
	(lambda (list)
		(if (equal? list (flatten-once-max list))
			list
			(completely-flatten (flatten-once-max list)))))

(define convert-ascii-list-to-string
	(lambda (ascii-list)
		(list->string (map integer->char ascii-list))))

(define get-value-of-const
	(lambda (const-record)
		(let ((raw-value (cddr const-record))
			(type (cadr const-record)))
			(cond 
				((eq? type 'T_PAIR) (cons (car raw-value) (cadr raw-value)))
				((eq? type 'T_VECTOR) (list->vector raw-value))
				((eq? type 'T_STRING) (convert-ascii-list-to-string raw-value))
				(#t  (car raw-value)))
			)))

(define find-in-const-list
	(lambda (type const const-list)
;		(display (format "looking for ~A of type ~A in ~A\n" const type const-list))
		(if (null? const-list)
			(begin (display (format "Did not find the const ~A of type ~A in the list\n" const type)) #f)
			(let ((const-record (car const-list)))
;				(display (format "Comparing ~A with ~A\n" const const-record))
				(if (and (equal? type (cadr const-record)) (equal? const (get-value-of-const const-record)))
					(car const-record)
					(find-in-const-list type const (cdr const-list)))))))

(define get-const-type
	(lambda (const)
;		(display (format "Finding the type of ~A\n" const))
		(cond 
			((eq? const (void)) 'T_VOID)
			((null? const) 'T_NIL)
			((integer? const) 'T_INTEGER)
			((number? const) 'T_FRACTION)
			((or (eq? #f const) (eq? #t const)) 'T_BOOL)
			((char? const) 'T_CHAR)
			((string? const) 'T_STRING)
			((symbol? const) 'T_SYMBOL)
			((pair? const) 'T_PAIR)
			((vector? const) 'T_VECTOR)
			(#t 'undef))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;		(display (format "Getting a const from: ~A\n" sexpr))
		(cond 
			((and (not (integer? sexpr)) (number? sexpr)) (list (numerator sexpr) (denominator sexpr) sexpr))
			((pair? sexpr) (append (extract-sub-consts-from-pair sexpr) (list sexpr)))
			((vector? sexpr) (append (extract-sub-consts-from-vector sexpr) (list sexpr)))
		    (#t sexpr))))

(define extract-sub-consts-from-vector
	(lambda (vec)
		(flatten-once-max
			(map
				get-consts-from-sexpr
				(vector->list vec)))))

(define extract-sub-consts-from-pair
	(lambda (p) 
;		(display (format "The sub pairs: ~A\n" (get-sub-pairs p)))
		(append 
			(flatten-once-max 
				(map
					get-consts-from-sexpr
					(completely-flatten p)))
			(get-sub-pairs p))))

(define get-sub-pairs
	(lambda (p)
		(letrec ((head-sub-pairs (if (pair? (car p)) (get-sub-pairs (car p)) (list (car p)))))
;		(display (format "Getting sub pairs of ~A, head-sub-pairs: ~A\n" p head-sub-pairs))
		(if (and (not (null? (cdr p))) (list? (cdr p)))
			(append (get-sub-pairs (cdr p)) head-sub-pairs (list (cons (car p) (cdr p))))
			(append head-sub-pairs (list p))))))

(define find-consts
	(lambda (parse-list)
;		(display (format "parse-list: ~A\n" parse-list)) (newline)
		(flatten-once-max 
			(map
				(lambda (sexpr) 
					(cond
						((eq? 'const (car sexpr)) (get-consts-from-sexpr (cadr sexpr)))
					    ((list? parse-list) (find-consts sexpr))))
				(filter (lambda (x) (and (list? x) (not (null? x)))) parse-list)))))

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
						(list (get-address) 'T_NIL '()))
					((eq? const (void))
						(list (get-address) 'T_VOID 0))
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
    					(append (list (get-address)) (list 'T_STRING) (convert-string-to-ascii-list const)))
    				((symbol? const)
    					(list (get-address) 'T_SYMBOL const))
    				((pair? const)
    					(list (get-address) 'T_PAIR (car const) (cdr const)))
    				((vector? const)
    					(append (list (get-address)) (list 'T_VECTOR) (vector->list const)))	
					(#t 'undef))))
				(cons const-label (create-const-list-helper (cdr const-list)))))))

(define convert-string-to-ascii-list
	(lambda (str)
		(map
			char->integer
			(string->list str))))

(define make-const-table
	(lambda (parsed-code)
;		(display (format "Result of flatten: ~A\n" (find-consts parsed-code)))
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
;		(display (format "Converting pair ~A\n" const-record))
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
					((pair? val) (find-in-const-list 'T_PAIR val const-list))
					((vector? val) (find-in-const-list 'T_VECTOR val const-list))
					((string? val) (find-in-const-list 'T_STRING val const-list))
					(#t (begin (display (format "the val ~A is not yet supported for assembly declaration" val)) #f))))
			vals-list)))

(define create-vector-creation-args
	(lambda (val-places)
		(let ((format-string (fold-left string-append " " (map (lambda (place) (format "const_~A, " place)) val-places))))
			(substring format-string 0 (- (string-length format-string) 2)))))

(define convert-vector-to-assembly
	(lambda (const-record const-list)
		(let ((vals-places (find-vector-vals-places (cddr const-record) const-list)))
			(string-append
				(format "const_~A:\n    MAKE_LITERAL_VECTOR" (car const-record))
				(create-vector-creation-args vals-places)
				"\n"))))

(define create-string-creation-args
	(lambda (ascii-list)
		(let ((format-string (fold-left string-append " " (map (lambda (ascii) (format "~A, " ascii)) ascii-list))))
			(substring format-string 0 (- (string-length format-string) 2)))))

(define convert-string-to-assembly
	(lambda (const-record const-list)
		(format "const_~A:\n    MAKE_LITERAL_STRING~A\n" 
			(car const-record) 
			(create-string-creation-args (cddr const-record)))))

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
							    ((equal? 'T_STRING type) (convert-string-to-assembly const-record const-table))
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

(define compile-scheme-file
	(lambda (in-file out-file)
		(let* 
			((parsed-scheme-code (pipeline (read-from-file in-file)))
			(constants-table (make-const-table parsed-scheme-code))
			(symbol-table (filter (lambda (x) #t) constants-table))
			(global-variable-table (make-global-variable-table parsed-scheme-code)))
;			(display (format "Constants table: ~A\n" constants-table))
;			(display (format "Parsed code: ~A\n" parsed-scheme-code))
;			(display (format "G-V-T: ~A\n" global-variable-table))
			(write-to-file out-file
				(string-append 
					prolog 
					(extract-const-table constants-table)
					(extract-fvar-table global-variable-table)
					(code-gen parsed-scheme-code constants-table global-variable-table)
					epilog
					))
			)))

(define get-fvar-address
	(let ((fvar-address -1))
		(lambda ()
			(set! fvar-address (+ 1 fvar-address))
			fvar-address)))

(define make-global-variable-table
	(lambda (code)
		(map
			(lambda (fvar-exp) (list (get-fvar-address) (cadadr fvar-exp)))
			(filter 
				(lambda (x) (and (list? x) (not (null? x)) (eq? 'define (car x)))) 
				code))))

(define extract-fvar-table
	(lambda (fvar-list)
		(fold-right
			string-append
			""
			(map
				(lambda (x) 
					(format "fvar_~A:\n    dq 0\n" (car x)))
				fvar-list))))