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
					((string? x) (find-type-in-list 'T_STRING (convert-string-to-ascii-list x) in-list))
					(#t (begin (display (format "code-gen: need to support ~A\n" x)) #f))))
			in-list)))

(define find-type-in-list
	(lambda (type const list)
		(if (or (null? list) (not (list? list)))
			(begin (display (format "const ~A not found in const-list\n" const)) 0)
			(let ((current-node (car list)))
;				(display (format "const: ~A, current-node: ~A\n" const current-node))
				(if (and (eq? type (cadr current-node)) (equal? const (cddr current-node)))
					(car current-node)
					(find-type-in-list type const (cdr list)))))))

(define code-gen-helper
	(lambda (code constants-table env-depth)
;		(display (format "code: ~A" code)) (newline)
		(cond 
			((equal? (car code) 'const) (code-gen-const (cadr code) constants-table))
			((equal? (car code) 'lambda-simple) (code-gen-lambda-simple code constants-table env-depth))
			((equal? (car code) 'seq) (code-gen-seq code constants-table env-depth))
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
											(code-gen-helper code-part constants-table 0)
											"    call write_sob_if_not_void\n    add rsp, 1*8\n"))
					scheme-code))
			)))
;			(display res)
			res)))

(define code-gen-const
	(lambda (const constants-table)
		(format "    push qword [const_~A]\n" (find-in-list const constants-table))))

(define code-gen-seq
	(lambda (seq-exp constants-table env-depth)
		(fold-right
			string-append
			""
			(map (lambda (e) (code-gen-helper e constants-table env-depth)) (cadr seq-exp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Lambda Code Gen;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-lambda-index
	(let ((index -1)) (lambda () (set! index (+ 1 index)) index)))

(define code-gen-lambda-simple
	(lambda (lam constants-table env-depth)
		(display (format "Generating code for lambda: ~A\n" lam))
		(let 
			((params-list (cadr lam))
			(lambda-index (get-lambda-index))
			(body (cddr lam)))
			(string-append
				(make-env params-list lambda-index env-depth)
				(string-append
					(format "B_~A:\n" lambda-index) 
					;(check-arg-count (length params-list))
					"    push rbp\n    mov rbp, rsp\n"	)
				(fold-left 
					string-append 
					""
					(map 
						(lambda (body-part) (code-gen-helper body-part constants-table (+ 1 env-depth))) 
						body))
				(format "    leave\n    ret\n\nL_~A:\n    push qword [rax]\n" lambda-index)))))

(define check-arg-count
	(lambda (arg-count)
		(format "    mov rax, qword[rbp + 3*8]
	cmp rax, ~A  
	jne END
"
		arg-count)))

(define env-mallocs
	(lambda (n env-depth)
		(format "    mov rdi, 8*~A 				; 8*n
	call malloc
	mov rcx, rax
	mov rdi, 8*~A 				; 8*(m + 1)
	call malloc
	mov rbx, rax
"
		n (+ 1 env-depth))))

(define make-env
	(lambda (params-list lambda-index env-depth)
		(let ((len (length params-list)))
			(string-append
				(env-mallocs len env-depth)
				(copy-params-from-stack lambda-index len)
				(copy-envs-from-stack lambda-index env-depth)
				(make-closure lambda-index)))))

(define copy-params-from-stack
	(lambda (index len)
		(format "	mov rax, 0
p_~A:
	cmp	rax, ~A 						; loading params in current lambda (n)
	je e_~A
	mov r10, rax
	add rax, 4
	sal rax, 3
	mov r8, rax
	add r8, rbp
	mov r9, rcx
	add r9, rax
	mov r9, qword[r8] 	; PARAMi
	mov rax, r10
	inc rax
	jmp p_~A"
		index len index index)))

(define copy-envs-from-stack
	(lambda (index env-depth)
		(format"
e_~A:
	mov [rbx], rcx
	mov rax, 0
e_loop_~A:	
	cmp rax, ~A ; setting up env extension (m)
	je CL_~A

	mov r10, rax
	add rax, 1
	sal rax, 3
	mov r8, rax
	add r8, rbp
	mov r9, r8
	add r9, 8
	mov rax, r10
	jmp e_loop_~A
"
		index index env-depth index index)))

(define make-closure
	(lambda (index)
		(format "
CL_~A:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_~A

	jmp L_~A
"
		index index index)))