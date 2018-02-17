(define code-header 
	"extern exit, printf, scanf, malloc
global main, write_sob, write_sob_if_not_void
section .text
main:
")

(define fvars 0)

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
					((symbol? x) (find-symbol x in-list))
					(#t (begin (display (format "code-gen: need to support ~A\n" x)) #f))))
			in-list)))

(define find-symbol
	(lambda (sym in-list)
		(let ((sym-string-ascii (convert-string-to-ascii-list (symbol->string sym))))
			(+ 1 (find-type-in-list 'T_STRING sym-string-ascii in-list)))))

(define find-in-fvar-list
	(lambda (fvar)
		(find-in-fvar-list-helper fvars fvar)))

(define find-in-fvar-list-helper
	(lambda (fvar-list fvar)
		(let ((current (car fvar-list)))
;			(display (format "looking for ~A\n" fvar))
			(if (eq? fvar (cadr current))
				(car current)
				(find-in-fvar-list-helper (cdr fvar-list) fvar)))))

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
			((equal? (car code) 'lambda-opt) (code-gen-lambda-opt code constants-table env-depth))
			((equal? (car code) 'pvar) (code-gen-pvar code))
			((and (equal? 'set (car code)) (equal? 'pvar (caadr code))) 
				(code-gen-set-pvar code constants-table env-depth))
			((equal? (car code) 'bvar) (code-gen-bvar code))
			((and (equal? 'set (car code))) (equal? 'bvar (caadr code))
				(code-gen-set-bvar code constants-table env-depth))
			((equal? 'or (car code)) (code-gen-or code constants-table env-depth))
			((equal? 'if3 (car code)) (code-gen-if code constants-table env-depth))
			((equal? (car code) 'seq) (code-gen-seq code constants-table env-depth))
			((equal? (car code) 'applic) (code-gen-applic code constants-table env-depth))
			((equal? (car code) 'tc-applic) (code-gen-applic code constants-table env-depth))
			((equal? (car code) 'define) (code-gen-define code constants-table env-depth))
			((equal? (car code) 'fvar) (code-gen-fvar code))
		    (#t (display (format "Code of type ~A is not yet supported" (car code)))))))

(define code-gen
	(lambda (scheme-code constants-table global-variable-table symbol-list)
		(set! fvars global-variable-table)
		(string-append
			code-header
			(create-symbol-linked-list symbol-list (length constants-table))
			(get-runtime-assembly-functions)
			(fold-right
				string-append
				""
				(map
					(lambda (code-part) (string-append 
											(code-gen-helper code-part constants-table 0)
											"    push rax\n    call write_sob_if_not_void\n    add rsp, 1*8\n"))
					scheme-code))
			)))

(define create-symbol-linked-list
	(lambda (symbol-list symbol-list-pointer)
		(string-append
			make-empty-list
			(create-symbol-list-node symbol-list)
			"  ;;;    FINISHED SYMBOL LIST ;;;;\n")))

(define make-empty-list
	"
	;;;;     Creating symbol list    ;;;;
	mov rax, symbol_table")

(define create-symbol-list-node
	(lambda (sym-addr-list)
		(if (null? sym-addr-list)
			""
			(string-append 
				(format "
	push rax
	mov rdi, 8
	call malloc
	mov r12, rax
	sub r12, start_of_data
	pop rbx
	mov r13, rbx
	mov rbx, qword [rbx]
	sar rbx, TYPE_BITS
	add rbx, r12
	sal rbx, TYPE_BITS
	mov qword [r13], rbx

	mov rbx, const_~A
	sub rbx, start_of_data
	sal rbx, 34				; magic number for DATA_UPPER
	mov qword [rax], rbx
		" (car sym-addr-list))
				(create-symbol-list-node (cdr sym-addr-list))))))

(define code-gen-const
	(lambda (const constants-table)
		(format "    mov rax, qword [const_~A]\n" (find-in-list const constants-table))))

(define code-gen-seq
	(lambda (seq-exp constants-table env-depth)
		(fold-right
			string-append
			""
			(map (lambda (e) (code-gen-helper e constants-table env-depth)) (cadr seq-exp)))))

(define get-or-index
	(let ((or-index -1)) (lambda () (set! or-index (+ 1 or-index)) or-index)))

(define code-gen-or
	(lambda (or-exp constants-table env-depth)
		(let ((index (get-or-index)))
		(fold-right
			string-append
			(format "    L_or_~A:\n" index)
			(map
				(lambda (or-part)
					(string-append
						(code-gen-helper or-part constants-table env-depth)
						(format "    cmp rax, SOB_FALSE\n    jne L_or_~A\n" index)))
				(cadr or-exp))))))

(define get-if-index
	(let ((if-index -1)) (lambda () (set! if-index (+ 1 if-index)) if-index)))

(define code-gen-if
	(lambda (if-exp constants-table env-depth)
		(let 
			((test (cadr if-exp))
			(test-true (caddr if-exp))
			(test-false (cadddr if-exp))
			(index (get-if-index)))
			(string-append
				(code-gen-helper test constants-table env-depth)
				(format "    cmp rax, SOB_FALSE\n    je  if_f_~A\n" index)
				(code-gen-helper test-true constants-table env-depth)
				(format "    jmp L_if_~A\nif_f_~A:\n" index index)
				(code-gen-helper test-false constants-table env-depth)
				(format "L_if_~A:\n" index)))))

(define code-gen-define
	(lambda (define-exp constants-table env-depth) 
		(string-append
			(format "\n    ;;;    ~A    ;;;;\n\n" (cadadr define-exp))
			(code-gen-helper (caddr define-exp) constants-table env-depth)
			(format "    mov qword [fvar_~A], rax\n" (find-in-fvar-list (cadadr define-exp)))
			"    mov rax, SOB_VOID\n")))

(define code-gen-fvar
	(lambda (fvar-exp)
		(format "    mov rax, qword [fvar_~A]\n" (find-in-fvar-list (cadr fvar-exp)))))

(define code-gen-pvar
	(lambda (pvar)
		(let ((minor (caddr pvar)))
			(format "    mov rax, qword [rbp + (4 + ~A)*8]\n" minor))))

(define code-gen-set-pvar
	(lambda (set-pvar constants-table env-depth)
		(let 
			((minor (car (cddadr set-pvar)))
			(exp (code-gen-helper (caddr set-pvar) constants-table env-depth)))
			(string-append
				exp
				(format "    mov qword [rbp + (4 + ~A)*8], rax\n    mov rax, SOB_VOID\n" minor)))))

(define code-gen-bvar
	(lambda (bvar)
		(let 
			((major (caddr bvar))
			(minor (cadddr bvar)))
			(format "
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + ~A*8]
	mov rax, qword [rax + ~A*8]
" major minor))))

(define code-gen-set-bvar
	(lambda (set-bvar constants-table env-depth)
		(let 
			((major (car (cddadr set-bvar))))
			(minor (cadr (cddadr set-bvar)))
			(exp (code-gen-helper (caddr set-bvar) constants-table env-depth))
			(display "set-bvar\n")
			(string-append
				exp
				(format "
	mov rbx, qword [rbp + 2*8]
	mov rbx, qword [rbx + ~A*8]
	mov qword [rbp + ~A*8], rax
	mov rax, SOB_VOID
" major minor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Lambda Code Gen;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-lambda-index
	(let ((index -1)) (lambda () (set! index (+ 1 index)) index)))

(define code-gen-lambda-simple
	(lambda (lam constants-table env-depth)
;		(display (format "Generating code for lambda: ~A\n" lam))
		(let 
			((params-list (cadr lam))
			(lambda-index (get-lambda-index))
			(body (cddr lam)))
			(string-append
				(make-env params-list lambda-index env-depth)
				(string-append
					(format "B_~A:\n" lambda-index) 
					"    push rbp\n    mov rbp, rsp\n")
				(fold-left 
					string-append 
					""
					(map 
						(lambda (body-part) (code-gen-helper body-part constants-table (+ 1 env-depth))) 
						body))
				(format "    leave\n    ret\n\nL_~A:\n    mov rax, qword [rax]\n" lambda-index)))))

(define code-gen-lambda-opt
	(lambda (lam constants-table env-depth)
		(let 
			((params-list (append (cadr lam) (list (caddr lam))))
			(lambda-index (get-lambda-index))
			(body (cdddr lam)))
			(string-append
				(make-env params-list lambda-index env-depth)
				(string-append
					(format "B_~A:\n" lambda-index) 
					"    push rbp\n    mov rbp, rsp\n")
				(fix-empty-variadic-var (length params-list) lambda-index)
				(fold-left 
					string-append 
					""
					(map 
						(lambda (body-part) (code-gen-helper body-part constants-table (+ 1 env-depth))) 
						body))
				(format "    leave\n    ret\n\nL_~A:\n    mov rax, qword [rax]\n" lambda-index)))))

(define env-mallocs
	(lambda (n env-depth)
		(format "    
	mov rdi, 8*~A 				; 8*n
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
			(if (= 0 env-depth)
				(first-lambda lambda-index)
				(string-append    ;; not of depth 0, need to copy vars
					(extend-envs env-depth lambda-index)
					(copy-params-from-stack lambda-index len)
					(make-closure lambda-index))))))

(define first-lambda
	(lambda (index)
		(format "
	mov rdi, 8
	call malloc
	mov r12, rax
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, r12, B_~A
	jmp L_~A
" index index)))

(define extend-envs
	(lambda (depth index)
		(format "
	mov rdi, 8*~A
	call malloc
	mov rbx, rax
	mov rcx, [rbp + 2*8]
	mov r9, ~A
	mov r8, 0

e_loop_~A:
	cmp r8, r9
	je  e_loop_end_~A
	mov r12, qword [rcx + r8*8]
	inc r8
	mov qword [rbx + 8*r8], r12
	jmp e_loop_~A
e_loop_end_~A:
" (+ 1 depth) (+ 1 depth) index index index index)))

(define copy-params-from-stack
	(lambda (index len)
		(format 
"	push rbx
	mov rdi, 8*~A
	call malloc
	mov rcx, rax
	pop rbx
	mov r8, 0
p_~A:
	cmp	r8, ~A 						; loading params in current lambda (n)
	je  p_loop_end_~A
	mov rdi, qword [rbp + 8*(r8 + 4)]
	mov qword [rcx + 8*r8], rdi
	inc r8
	jmp p_~A

p_loop_end_~A:
"
		len index len index index index)))

(define copy-envs-from-stack
	(lambda (index env-depth)
		(format"
e_~A:
	mov [rbx], rcx
	mov r9, 0
	mov r8, qword [rbp + 2*8]

e_loop_~A:	
	cmp r9, ~A ; setting up env extension (m)
	je  CL_~A
	mov rdx, qword [r8 + 8*r9]
	inc r9
	mov qword [rbx + 8*r9], rdx
	jmp e_loop_~A
"
		index index env-depth index index)))

(define fix-empty-variadic-var
	(lambda (num-of-params index)
		(format "
	mov r8, qword [rbp + 3*8]
	cmp r8, ~A
	jle N_F_~A

	mov r9, r8
	sub r9, ~A		; number of params for variadic var = qword [rbp + 3*8] - num-of-params
	mov r10, 0
N_F_loop_~A:
	cmp r10, r9
	je  N_F_~A
	mov r14, r8
	add r14, 3
	sal r14, 3 				
	add r14, rbp 			
	mov r13, qword [r14]	; r14 = rbp + (i+4)*8
	mov rdi, 8
	call malloc
	mov qword[rax], r13
	mov r13, rax

	sub r14, 8				
	mov r12, qword [r14]	; r14 = rbp + (i+3)*8
	mov rdi, 8
	call malloc
	mov qword[rax], r12
	mov r12, rax
	mov rdi, 8
	call malloc
	MAKE_MALLOC_LITERAL_PAIR rax, r12, r13	
	mov rax, qword [rax]
	mov qword [r14], rax
	dec r8
	inc r10
	jmp N_F_loop_~A

N_F_~A:
" num-of-params index num-of-params index index index index)))

(define make-closure
	(lambda (index)
		(format "
CL_~A:
	mov qword [rbx], rcx
	push rbx
	mov rdi, 16
	call malloc
	pop rbx
	MAKE_LITERAL_CLOSURE rax, rbx, B_~A

	jmp L_~A
"
		index index index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Applic Code Gen;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define apply-proc
"	mov r12, rax			; check we are applying closure
	TYPE r12
	cmp r12, T_CLOSURE
	jne END

	mov rbx, rax			; apply the closure
	CLOSURE_ENV rbx
	CLOSURE_CODE rax
	push rbx
")

(define code-gen-applic
	(lambda (app const-table env-depth)
		(let 
			((params-list (caddr app))
			(proc (cadr app)))
			(string-append
				(code-gen-applic-params params-list const-table env-depth)
				(code-gen-helper proc const-table env-depth)
				apply-proc
				(format "    call rax\n    add rsp, ~A*8\n" (+ 3 (length params-list)))))))

(define code-gen-applic-params
	(lambda (params-list const-table env-depth)
		(let
			((num-of-params (+ 1 (length params-list))))
			(string-append
				"    push SOB_NIL\n"
				(fold-right
					string-append
					""
					(map
						(lambda (param)
							(string-append (code-gen-helper param const-table env-depth) "    push rax\n"))
						(reverse params-list)))
				(format "    push ~A\n" num-of-params)))))

(define code-gen-tc-applic
	(lambda (app const-table env-depth)
		(let 
			((params-list (caddr app))
			(proc (cadr app)))
			(string-append
				(code-gen-applic-params params-list const-table env-depth)
				(code-gen-helper proc const-table env-depth)
				apply-proc
				(fix-stack (length params-list))
				(format "    add rsp,~A*8\n" (+ 1 (length params-list)))))))

(define fix-stack
	(lambda (num-of-params)
		(format "
	mov r8, rbp
	mov r9, rsp
	leave
	mov rsp, r9

	mov r9, r8
	add r9, 8 * (~A + 4)
	mov r10, 

	" num-of-params)))