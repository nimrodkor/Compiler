
(define set-up-global-env-with-one-var
"	mov rdi, 8
	call malloc
	mov rcx, rax
	mov rdi, 8
	call malloc
	mov rbx, rax
	mov rax, 4*3
	add rax, rbp
	add rcx, rax
	mov rcx, qword[rax]
	mov [rbx], rcx
	mov rax, 8
	add rbp, rax
	mov rax, rbp
	add rax, 8
")

(define not-assembly
(string-append "\n;;  not? ;;\n" set-up-global-env-with-one-var "
CL_not:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_not

	jmp L_not
B_not:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	cmp rax, SOB_FALSE
	je  B_not_false
	mov rax, SOB_FALSE
	jmp B_not_ret
B_not_false:
	mov rax, SOB_TRUE
B_not_ret:
    leave
    ret

L_not:
    mov rax, qword [rax]
    mov qword [fvar_0], rax
    mov rax, SOB_VOID
"))

(define car-assembly
	(string-append "\n;;  car ;;\n" set-up-global-env-with-one-var
"
CL_car:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_car

	jmp L_car
B_car:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	CAR rax
    leave
    ret

L_car:
    mov rax, qword [rax]
    mov qword [fvar_1], rax
    mov rax, SOB_VOID
"))

(define cdr-assembly
	(string-append "\n;;  cdr ;;\n"set-up-global-env-with-one-var
"
CL_cdr:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_cdr

	jmp L_cdr
B_cdr:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	CDR rax
    leave
    ret

L_cdr:
    mov rax, qword [rax]
    mov qword [fvar_2], rax
    mov rax, SOB_VOID
"))

(define char?-assembly
	(string-append "\n;;  char? ;;\n"set-up-global-env-with-one-var
"
CL_char:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_char

	jmp L_char
B_char:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	TYPE rax
	cmp rax, T_CHAR
	je  B_char_T
	mov rax, SOB_FALSE
	jmp B_char_end
B_char_T:
	mov rax, SOB_TRUE
B_char_end:
    leave
    ret
L_char:
    mov rax, qword [rax]
    mov qword [fvar_3], rax
    mov rax, SOB_VOID
"))

(define integer?-assembly
	(string-append "\n;;  integer? ;;\n"set-up-global-env-with-one-var
"
CL_integer:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_integer

	jmp L_integer
B_integer:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	TYPE rax
	cmp rax, T_INTEGER
	je  B_integer_T
	mov rax, SOB_FALSE
	jmp B_integer_end
B_integer_T:
	mov rax, SOB_TRUE
B_integer_end:
    leave
    ret
L_integer:
    mov rax, qword [rax]
    mov qword [fvar_4], rax
    mov rax, SOB_VOID
"))

(define null?-assembly
	(string-append "\n;;  null? ;;\n"set-up-global-env-with-one-var
"
CL_null:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_null

	jmp L_null
B_null:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	TYPE rax
	cmp rax, T_NIL
	je  B_null_T
	mov rax, SOB_FALSE
	jmp B_null_end
B_null_T:
	mov rax, SOB_TRUE
B_null_end:
    leave
    ret
L_null:
    mov rax, qword [rax]
    mov qword [fvar_5], rax
    mov rax, SOB_VOID
"))

(define number?-assembly
	(string-append "\n;;  number? ;;\n" set-up-global-env-with-one-var
"
CL_number:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_number

	jmp L_number
B_number:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	TYPE rax
	cmp rax, T_INTEGER
	je  B_number_T
	cmp rax, T_FRACTION
	je B_number_T
	mov rax, SOB_FALSE
	jmp B_number_end
B_number_T:
	mov rax, SOB_TRUE
B_number_end:
    leave
    ret
L_number:
    mov rax, qword [rax]
    mov qword [fvar_6], rax
    mov rax, SOB_VOID
"))

(define pair?-assembly
	(string-append "\n;;  pair? ;;\n" set-up-global-env-with-one-var
"
CL_pair:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_pair

	jmp L_pair
B_pair:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	TYPE rax
	cmp rax, T_PAIR
	je  B_pair_T
	mov rax, SOB_FALSE
	jmp B_pair_end
B_pair_T:
	mov rax, SOB_TRUE
B_pair_end:
    leave
    ret
L_pair:
    mov rax, qword [rax]
    mov qword [fvar_7], rax
    mov rax, SOB_VOID
"))

(define procedure?-assembly
	(string-append "\n;;  procedure? ;;\n" set-up-global-env-with-one-var
"
CL_procedure:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_procedure

	jmp L_procedure
B_procedure:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	TYPE rax
	cmp rax, T_CLOSURE
	je  B_procedure_T
	mov rax, SOB_FALSE
	jmp B_pair_end
B_procedure_T:
	mov rax, SOB_TRUE
B_procedure_end:
    leave
    ret
L_procedure:
    mov rax, qword [rax]
    mov qword [fvar_8], rax
    mov rax, SOB_VOID
"))

;(define set-car!-assembly
;	(string-append "\n;;  set-car! ;;\n" set-up-global-env-with-one-var
;"
;CL_set_car:
;	mov rdi, 16
;	call malloc
;	MAKE_LITERAL_CLOSURE rax, rbx, B_set_car

;	jmp L_set_car
;B_set_car:
;    push rbp
;    mov rbp, rsp
;    mov rax, qword [rbp + 4*8]
;	CAR rax
;	mov rbx, qword [rbp + 5*8]
;	mov [rax], rbx
;	mov rax, SOB_VOID
;    leave
;    ret
;L_set_car:
;    mov rax, qword [rax]
;    mov qword [fvar_9], rax
;    mov rax, SOB_VOID
;"))

(define get-runtime-assembly-functions
	(lambda ()
		(string-append
			"\n;; Start of library functions \n\n"
			not-assembly
			car-assembly
			cdr-assembly
			char?-assembly
			integer?-assembly
			null?-assembly
			number?-assembly
			pair?-assembly
			procedure?-assembly
			;set-car!-assembly
			"\n;; End of library functions \n\n")))


(define get-scheme-impls
"")
;"
;(define zero? (lambda (x) (= 0 x)))
;(define not (lambda (x) (if (eq? x #f) #t #f)))
;(define map (lambda (func lst) (if (null? lst) '() (cons (func (car lst)) (map func (cdr lst))))))

;(define length (lambda (lst) (if (null? lst) 0 (+ 1 (length (cdr lst))))))

;(define regular-append
;     (lambda (l m)
;         (if (null? l)
;             m
;             (cons (car l) (regular-append (cdr l) m)))))
             
;(define append-helper
;    (lambda (x y)
;        (if (null? y)
;            x
;            (regular-append x (append-helper (car y) (cdr y))))))

;(define append
;    (lambda lst
;       	(cond 
;	        ((null? lst) '())
;	        ((null? (cdr lst)) (car lst))
;	        ((= 2 (cdr (cdr lst))) (regular-append (car lst) (car (cdr lst))))
;	        (else 
;	            (append-helper (car lst) (cdr lst))))))
;")

(define boolean?-assembly
"
p_bool:
	cmp	rax, 1 						; loading params in current lambda (n)
	je e_0
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
	jmp p_bool

CL_bool:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_bool

	jmp L_bool
B_bool:
	push rbp
    mov rbp, rsp
    mov rax, qword[rbp + 4*8]
    TYPE rax
    cmp rax, T_BOOL
    jmp true
    mov rax, qword[const_2]
    jmp L_bool
true:
	mov rax, qword[const_3]
L_bool:
	leave
	ret
	")

