;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;	Helpers	  ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define is-lambda?
	(lambda (e)
		(if (and (list? e) (not (null? e)))
			(or (equal? (car e) 'lambda-simple)
				(equal? (car e) 'lambda-opt)
				(equal? (car e) 'lambda-var))
			#f)))


(define get-lambda-params
	(lambda (lam)
;		(display "g-l-p: ") (display lam) (newline)
		(if (and (list? lam) (< 1 (length lam)))
			(cond ((equal? (car lam) 'lambda-simple) (list (cadr lam)))
				((equal? (car lam) 'lambda-opt)
					(list (cadr lam) (caddr lam)))
				(#t '()))
			'())))

(define get-lambda-body
	(lambda (lam)
		(if (equal? 'lambda-opt (car lam))
			(cdddr lam) 
			(cddr lam))))

(define need-to-box?
	(lambda (var boxed-params bounded-params)
;    	(display (list "var:" var "boxed-params:" boxed-params "bounded-params:" bounded-params)) (newline)
		(and (contains? boxed-params var)
			(not (contains? bounded-params var)))))

(define contains?
	(lambda (nested-list x)
;		(display (list "contains - nested-list" nested-list)) (newline)
		(if (null? nested-list)
			#f
			(or (if (list? (car nested-list))
					(member x (car nested-list))
					(equal? x (car nested-list)))
				(contains? (cdr nested-list) x)))))

(define index-of
	(lambda (x list num)
;		(display "list: ") (display list) (display " x: ") (display x) (newline)
		(cond ((null? list) -1)
			((equal? x (car list)) num)
			(#t (index-of x (cdr list) (+ 1 num))))))

(define is-last?
	(lambda (x list)
		(equal? x (car (reverse list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;	remove-applic-lambda-nil	  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remove-applic-lambda-nil
	(lambda (e) 
		(if (or (null? e) (not (list? e)))
			e
			(if (and 
					(equal? (car e) 'applic)
					(equal? (caadr e) 'lambda-simple)
					(null? (cadadr e))
					(null? (caddr e)))
				(remove-applic-lambda-nil (car (cddadr e)))
				(map remove-applic-lambda-nil e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;		box-set	  		  	  	 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define box-set
	(lambda (e)
		(box-set-helper e '() '())))

(define box-set-helper
	(lambda (e boxed-params bounded-params-without-box)
;		(display (list "boxed-params:" boxed-params)) (newline)
		(let* ((params-to-box (if (is-lambda? e) (get-params-to-box e) '()))
			(all-bounded-params
  				(if (is-lambda? e)
					(append bounded-params-without-box (fix-lambda-opt-vars (get-lambda-params e)))
					bounded-params-without-box)))
		(cond
			((or (null? e) (not (list? e))) e)
			((and (is-lambda? e) (not (null? params-to-box)))
				(add-box-set-to-lambda e params-to-box (append boxed-params params-to-box)))
			((equal? (car e) 'set)
				(replace-set-with-box-set e boxed-params all-bounded-params))
			((equal? (car e) 'var)
				(replace-get-with-box-get e boxed-params all-bounded-params))
			(#t (map (lambda (x) (box-set-helper x boxed-params all-bounded-params)) e)))
		)))

(define add-box-set-to-lambda
	(lambda (lam params-to-box already-boxed-params)
;		(display "add-box-set-to-lambda: ") (display lam) (display " ") (display params-to-box) (newline)
;		(display (list "params-to-box:" params-to-box "already-boxed-params:" already-boxed-params)) (newline)
		(let* 
			((params (get-lambda-params lam))
			(body (get-lambda-body lam))
			(box-set-list (create-box-set-list params-to-box)))
			(append
				(list (car lam))
				params
				(list
					(list
						'seq
						(append box-set-list
							(if (equal? (caar body) 'seq)
								(box-set-helper (cadar body) (append (list params-to-box) already-boxed-params) '() )
								(box-set-helper body (append params-to-box already-boxed-params) '())))
					)))
			)))

(define get-params-to-box
	(lambda (e)
;		(display "get-params-to-box: ") (display e) (newline)
		(let ((lam-body (get-lambda-body e)))
			(filter
				(lambda (parameter)
					(and
						(is-bound? parameter lam-body)
						(is-set? parameter lam-body)
						(is-get? parameter lam-body)))
			(fix-lambda-opt-vars (get-lambda-params e))))))

(define create-box-set-list
	(lambda (params)
		(map (lambda (x) (list 'set (list 'var x) (list 'box (list 'var x)))) params)))

(define is-bound?
	(lambda (param lambda-body)
		(if (or (null? lambda-body) (not (list? lambda-body)))
			#f
			(ormap (lambda (lam) (is-bounded-in-lambda param lam)) (get-lambdas lambda-body)))))

(define get-lambdas
	(lambda (lam-body)
		(if (or (not (list? lam-body)) (null? lam-body))
			'()
			(if (is-lambda? lam-body)
				(append lam-body (get-lambdas (get-lambda-body lam-body)))
				(map get-lambdas lam-body)))))

(define is-bounded-in-lambda
	(lambda (param lam)
		(if (contains? (get-lambda-params lam) param)
			#f
			(ormap
				(lambda (x)
					(if (or (null? x) (not (list? x)))
						#f
						(if (equal? x (list 'var param))
							#t
							(is-bounded-in-lambda param x))))
				lam))))

(define is-set?
	(lambda (param lambda-body)
;		(display "is-set? ") (display lambda-body) (newline)
		(if (or (null? lambda-body) (not (list? lambda-body)))
			#f
			(if (and (is-lambda? lambda-body) (contains? (get-lambda-params lambda-body) param))
				#f
				(if (and (equal? 'set (car lambda-body)) (equal? (cadr lambda-body) (list 'var param)))
					#t
					(ormap (lambda (x) (is-set? param x)) lambda-body))))))

(define is-get?
	(lambda (param lambda-body)
;		(display "i-g: ") (display lambda-body) (newline)
		(if (or (null? lambda-body) (not (list? lambda-body)))
			#f
			(if (member param (get-lambda-params lambda-body))
				#f
				(if (equal? lambda-body (list 'var param))
					#t
					(if (equal? (car lambda-body) 'set)
						(is-get? param (caddr lambda-body))
						(ormap (lambda (x) (is-get? param x)) lambda-body)))))))

(define replace-set-with-box-set
	(lambda (e boxed-params all-bounded-params)
;		(display (list "replace-set:" e "boxed:" boxed-params "all:" all-bounded-params)) (newline)
		(let* ((var (cadadr e))
			(box-set-or-set
				(if (need-to-box? var boxed-params all-bounded-params) 'box-set (car e))))
		(append
			(list box-set-or-set (cadr e))
			(box-set-helper (cddr e) boxed-params all-bounded-params)))))

(define replace-get-with-box-get
	(lambda (e boxed-params all-bounded-params)
;		(display "replace-get: ") (newline)
		(if (need-to-box? (cadr e) boxed-params all-bounded-params)
			(list 'box-get e)
			e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;		pe->lex-pe 		  	  	 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pe->lex-pe
	(lambda (e)
		(pe->lex-pe-helper e (list '()))))

(define pe->lex-pe-helper
	(lambda (e nested-vars-list)
;		(display "vars list: ") (display nested-vars-list) (newline)
		(cond
			((or (null? e) (not (list? e))) e)
			((equal? (car e) 'var) (tag e nested-vars-list))
			((is-lambda? e) (handle-lambda-pe e nested-vars-list))
			(#t (map (lambda (x) (pe->lex-pe-helper x nested-vars-list)) e)))
		))

(define handle-lambda-pe
	(lambda (lam outside-params-list)
		(let* ((lambda-name (car lam))
			(lambda-vars (get-lambda-params lam))
			(lambda-body (get-lambda-body lam)))
;		(display (list "h-p: " lam lambda-vars)) (newline)
		(append
			(list lambda-name)
			lambda-vars
			(pe->lex-pe-helper
				lambda-body
				(append (list (fix-lambda-opt-vars lambda-vars)) outside-params-list))))))

(define fix-lambda-opt-vars
	(lambda (lam-vars)
		(if (= 1 (length lam-vars))
			(car lam-vars)
			(if (null? (car lam-vars))
				(list (cadr lam-vars))
				(append (car lam-vars) (list (cadr lam-vars)))))))

(define tag
	(lambda (var nested-vars-list)
;		(display (list "var: " var ", nested: " nested-vars-list)) (newline)
		(let 
			((var (cadr var))
			 (params (car nested-vars-list))
			 (bounded-params (cdr nested-vars-list)))
			(if (not (contains? nested-vars-list var))
				(list 'fvar var)
				(if (contains? (list params) var)
					(list 'pvar var (index-of var params 0))
					(get-bvar-tag var bounded-params 0))))))

(define get-bvar-tag
	(lambda (var nested-bounded-vars-list major)
;		(display (list "var: " var "var-list: " nested-bounded-vars-list)) (newline)
		(if (contains? (list (car nested-bounded-vars-list)) var)
			(list 'bvar var major (index-of var (car nested-bounded-vars-list) 0))
			(get-bvar-tag
				var
				(cdr nested-bounded-vars-list)
				(+ 1 major))
		)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;		annotate-tc		  	  	 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define annotate-tc
	(lambda (e)
		(cond ((or (null? e) (not (list? e))) e)
			((is-lambda? e)
			(append  (list (car e)) (get-lambda-params e) (check-for-tc (get-lambda-body e)))) 
			(#t (map annotate-tc e)))))

(define check-for-tc
	(lambda (lam)
		(cond
			((or (null? lam) (not (list? lam))) lam)
			((equal? (car lam) 'applic) (append (list 'tc-applic) (annotate-tc (cdr lam))))
			((equal? (car lam) 'if3) (append (list 'if3) (list (annotate-tc (cadr lam))) (check-for-tc (cddr lam))))
			((or (equal? (car lam) 'define) (equal? (car lam) 'set) (equal? (car lam) 'box-set))
				(map annotate-tc lam))
			((or (equal? (car lam) 'seq) (equal? (car lam) 'or))
				(append (list (car lam))
					(list (reverse (append (list (check-for-tc (car (reverse (cadr lam)))))
						(reverse (map annotate-tc (reverse (cdr (reverse (cadr lam)))))))))))
			(#t (map check-for-tc lam)))))