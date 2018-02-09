(define find-in-list
	(lambda (x in-list)
;		(display (format "looking for ~A in ~A\n" x in-list))
		(ormap
			(lambda (const-record)
;				(display (format "comparing ~A with ~A\n" x const-record))
				(cond 
					((eq? (void) x) 0)
					((eq? '() x) 1)
					((eq? x #f) 2)
					((eq? x #t) 3)
					((integer? x) (find-type-in-list 'T_INTEGER (list x) in-list))
					((number? x) (find-type-in-list 'T_FRACTION (list (numerator x) (denominator x)) in-list))
					((char? x) (find-type-in-list 'T_CHAR (list (char->integer x)) in-list))
					((pair? x) (find-type-in-list 'T_PAIR (list (car x) (cdr x)) in-list))
					((vector? x) (find-type-in-list 'T_VECTOR (vector->list x) in-list))
					(#t (begin (display (format "code-gen: need to support ~A\n" x)) #f))))
			in-list)))

(define find-type-in-list
	(lambda (type const list)
		(if (or (null? list) (not (list? list)))
			(begin (display (format "const ~A not found in const-list\n" const)) 0)
			(let ((current-node (car list)))
;				(display (format "current-node: ~A\n" current-node))
				(if (and (eq? type (cadr current-node)) (equal? const (cddr current-node)))
					(car current-node)
					(find-type-in-list type const (cdr list)))))))

(define code-gen-helper
	(lambda (code constants-table)
;		(display (format "code: ~A" code)) (newline)
		(cond ((equal? (car code) 'const) (code-gen-const (cadr code) constants-table))
		      (#t (display (format "Code of type ~A is not yet supported" (car code)))))))

(define code-gen
	(lambda (scheme-code constants-table)
		(let ((res (string-append
			code-header
			(fold-right
				string-append
				""
				(map
					(lambda (code-part) (string-append 
											(code-gen-helper code-part constants-table)
											"    call write_sob_if_not_void\n    add rsp, 1*8\n"))
					scheme-code))
			)))
;			(display res)
			res)))

(define code-gen-const
	(lambda (const constants-table)
		(format "    push qword [const_~A]\n" (find-in-list const constants-table))))