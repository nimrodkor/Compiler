
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

(define set-up-global-env-with-two-vars
	(lambda (tag)
	(format "
	mov rdi, 8*2
	call malloc
	mov rcx, rax
	mov rdi, 8*1 				
	call malloc
	mov rbx, rax
	mov rax, 0
p_~A:
	cmp	rax, 2				
	je  e_~A
	mov r10, rax
	add rax, 4
	sal rax, 3
	mov r8, rax
	add r8, rbp
	mov r9, rcx
	add r9, rax
	mov r9, qword[r8]
	mov rax, r10
	inc rax
	jmp p_~A
e_~A:
	mov [rbx], rcx
" tag tag tag tag)))

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
	jmp B_procedure_end
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

(define string?-assembly
	(string-append "\n;;  string? ;;\n" set-up-global-env-with-one-var
"
CL_string:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_string

	jmp L_string
B_string:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	TYPE rax
	cmp rax, T_STRING
	je  B_string_T
	mov rax, SOB_FALSE
	jmp B_string_end
B_string_T:
	mov rax, SOB_TRUE
B_string_end:
    leave
    ret
L_string:
    mov rax, qword [rax]
    mov qword [fvar_9], rax
    mov rax, SOB_VOID
"))

(define symbol?-assembly
(string-append "\n;;  symbol? ;;\n" set-up-global-env-with-one-var
"
CL_symbol:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_symbol

	jmp L_symbol
B_symbol:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	TYPE rax
	cmp rax, T_SYMBOL
	je  B_symbol_T
	mov rax, SOB_FALSE
	jmp B_symbol_end
B_symbol_T:
	mov rax, SOB_TRUE
B_symbol_end:
    leave
    ret
L_symbol:
    mov rax, qword [rax]
    mov qword [fvar_10], rax
    mov rax, SOB_VOID
"))

(define vector?-assembly
	(string-append "\n;;  vector? ;;\n" set-up-global-env-with-one-var
"
CL_vector:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_vector

	jmp L_vector
B_vector:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
	TYPE rax
	cmp rax, T_VECTOR
	je  B_vector_T
	mov rax, SOB_FALSE
	jmp B_vector_end
B_vector_T:
	mov rax, SOB_TRUE
B_vector_end:
    leave
    ret
L_vector:
    mov rax, qword [rax]
    mov qword [fvar_10], rax
    mov rax, SOB_VOID
"))

(define set-car!-assembly
	(string-append "\n;;   set-car!   ;;\n" (set-up-global-env-with-two-vars "set_car")
"
CL_set_car:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_set_car

	jmp L_set_car
B_set_car:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
    TYPE rax
    cmp rax, T_PAIR
    jne END

    mov rbx, qword [rbp + 5*8]
    mov rax, qword [rbp + 4*8]
    DATA_UPPER rax
    add rax, start_of_data
    mov qword [rax], rbx
    mov rax, SOB_VOID
    leave
    ret

L_set_car:
    mov rax, qword [rax]
    mov qword [fvar_12], rax
    mov rax, SOB_VOID
"))

(define set-cdr!-assembly
	(string-append "\n;;   set-cdr!   ;;\n" (set-up-global-env-with-two-vars "set_cdr")
"
CL_set_cdr:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_set_cdr

	jmp L_set_cdr
B_set_cdr:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
    TYPE rax
    cmp rax, T_PAIR
    jne END

    mov rbx, qword [rbp + 5*8]
    mov rax, qword [rbp + 4*8]
    DATA_LOWER rax
    add rax, start_of_data
    mov qword [rax], rbx
    mov rax, SOB_VOID
    leave
    ret

L_set_cdr:
    mov rax, qword [rax]
    mov qword [fvar_13], rax
    mov rax, SOB_VOID
"))

(define equals-assembly
	(string-append set-up-global-env-with-one-var 
"
CL_equals:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_equals

	jmp L_equals
B_equals:
    push rbp
    mov rbp, rsp

	mov r8, qword [rbp + 3*8]
	cmp r8, 1
	jge N_F_equals

	inc r8
	mov qword [rbp + 3*8], r8
	push SOB_NIL

N_F_equals:
    mov r8, qword [rbp + 3*8] ; get actual num of params
    mov rax, 0
    mov r9, qword [rbp + 4*8]
equals_loop:
    inc rax
    cmp rax, r8
    je  equals_B_true
    mov rbx, rax
    add rbx, 4
    shl rbx, 3
    mov r10, qword [rbp + rbx]	; rbx = (rax + 4)*8
    cmp r9, r10
    jne equals_B_false
    jmp equals_loop
equals_B_true:
	mov rax, SOB_TRUE
    jmp equals_B_end
equals_B_false:
	mov rax, SOB_FALSE
equals_B_end:
    leave
    ret

L_equals:
    mov rax, qword [rax]
    mov qword [fvar_14], rax
    mov rax, SOB_VOID
"))

(define greater-than-assembly
	(string-append set-up-global-env-with-one-var 
"
CL_gt:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_gt

	jmp L_gt
B_gt:
    push rbp
    mov rbp, rsp

	mov r8, qword [rbp + 3*8]
	cmp r8, 1
	jge N_F_gt

	inc r8
	mov qword [rbp + 3*8], r8
	push SOB_NIL

N_F_gt:
    mov r8, qword [rbp + 3*8] ; get actual num of params
    mov rax, 0
    mov r9, qword [rbp + 4*8]
gt_loop:
    inc rax
    cmp rax, r8
    je  gt_B_true
    mov rbx, rax
    add rbx, 4
    shl rbx, 3
    mov r10, qword [rbp + rbx]	; rbx = (rax + 4)*8
    cmp r9, r10
    jle gt_B_false
    mov r9, r10
    jmp gt_loop
gt_B_true:
	mov rax, SOB_TRUE
    jmp gt_B_end
gt_B_false:
	mov rax, SOB_FALSE
gt_B_end:
    leave
    ret

L_gt:
    mov rax, qword [rax]
    mov qword [fvar_15], rax
    mov rax, SOB_VOID
"))

(define less-than-assembly
	(string-append set-up-global-env-with-one-var 
"
CL_lt:
	mov rdi, 16
	call malloc
	MAKE_LITERAL_CLOSURE rax, rbx, B_lt

	jmp L_lt
B_lt:
    push rbp
    mov rbp, rsp

	mov r8, qword [rbp + 3*8]
	cmp r8, 1
	jge N_F_lt

	inc r8
	mov qword [rbp + 3*8], r8
	push SOB_NIL

N_F_lt:
    mov r8, qword [rbp + 3*8] ; get actual num of params
    mov rax, 0
    mov r9, qword [rbp + 4*8]
lt_loop:
    inc rax
    cmp rax, r8
    je  gt_B_true
    mov rbx, rax
    add rbx, 4
    shl rbx, 3
    mov r10, qword [rbp + rbx]	; rbx = (rax + 4)*8
    cmp r9, r10
    jge lt_B_false
    mov r9, r10
    jmp lt_loop
lt_B_true:
	mov rax, SOB_TRUE
    jmp gt_B_end
lt_B_false:
	mov rax, SOB_FALSE
lt_B_end:
    leave
    ret

L_lt:
    mov rax, qword [rax]
    mov qword [fvar_16], rax
    mov rax, SOB_VOID
"))

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
			string?-assembly
			symbol?-assembly
			vector?-assembly
			set-car!-assembly
			set-cdr!-assembly
			equals-assembly
			greater-than-assembly
			less-than-assembly
			"\n;; End of library functions \n\n")))


(define get-scheme-impls
"")
;"
;(define numerator car)
;(define denominator cdr)
;(define zero? (lambda (x) (and (number? x) (= 0 x))))
;(define rational? (lambda (x) (and (number? x) (not (integer? x))))
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

