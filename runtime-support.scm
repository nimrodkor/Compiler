(define set-up-global-env
"   
    mov rdi, 8
    call malloc 
    mov r12, rax
")

(define not-assembly
(string-append "\n;;  not? ;;\n" set-up-global-env "
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
    (string-append "\n;;  car ;;\n" set-up-global-env
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
    (string-append "\n;;  cdr ;;\n" set-up-global-env
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
    (string-append "\n;;  char? ;;\n" set-up-global-env
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
    (string-append "\n;;  integer? ;;\n" set-up-global-env
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

(define boolean?-assembly
    (string-append "\n;;  boolean? ;;\n" set-up-global-env
"
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
    je  B_bool_true
    mov rax, qword[const_2]
    jmp B_bool_end
B_bool_true:
    mov rax, qword[const_3]
B_bool_end:
    leave
    ret
L_bool:
    mov rax, qword [rax]
    mov qword [fvar_20], rax
    mov rax, SOB_VOID
    "))

(define null?-assembly
    (string-append "\n;;  null? ;;\n" set-up-global-env
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
    (string-append "\n;;  number? ;;\n" set-up-global-env
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
    (string-append "\n;;  pair? ;;\n" set-up-global-env
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
    (string-append "\n;;  procedure? ;;\n" set-up-global-env
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
    (string-append "\n;;  string? ;;\n" set-up-global-env
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
(string-append "\n;;  symbol? ;;\n" set-up-global-env
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
    (string-append "\n;;  vector? ;;\n" set-up-global-env
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
    (string-append "\n;;   set-car!   ;;\n" set-up-global-env
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
    (string-append "\n;;   set-cdr!   ;;\n" set-up-global-env
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
    (string-append "\n;;   =   ;;\n" set-up-global-env 
"
CL_equals:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_equals

    jmp L_equals
B_equals:
    push rbp
    mov rbp, rsp

    mov r8, qword [rbp + 3*8] ; get actual num of params
    sub r8, 1                 ; ignore '() param
    mov rax, 0
    mov r9, qword [rbp + 4*8]
equals_loop:
    inc rax
    cmp rax, r8
    je  equals_B_true
    mov rbx, rax
    add rbx, 4
    shl rbx, 3
    mov r10, qword [rbp + rbx]  ; rbx = (rax + 4)*8
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
    (string-append set-up-global-env 
"
CL_gt:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_gt

    jmp L_gt
B_gt:
    push rbp
    mov rbp, rsp

    mov r8, qword [rbp + 3*8] ; get actual num of params
    sub r8, 1                 ; ignore '() param
    mov rax, 0
    mov r9, qword [rbp + 4*8]
gt_loop:
    inc rax
    cmp rax, r8
    je  gt_B_true
    mov rbx, rax
    add rbx, 4
    shl rbx, 3
    mov r10, qword [rbp + rbx]  ; rbx = (rax + 4)*8
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
    (string-append  "\n;;   <   ;;\n" set-up-global-env 
"
CL_lt:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_lt

    jmp L_lt
B_lt:
    push rbp
    mov rbp, rsp

    mov r8, qword [rbp + 3*8] ; get actual num of params
    sub r8, 1                 ; ignore '() param
    mov rax, 0
    mov r9, qword [rbp + 4*8]
lt_loop:
    inc rax
    cmp rax, r8
    je  gt_B_true
    mov rbx, rax
    add rbx, 4
    shl rbx, 3
    mov r10, qword [rbp + rbx]  ; rbx = (rax + 4)*8
    cmp r9, r10
    jge lt_B_false
    mov r9, r10
    jmp lt_loop
lt_B_true:
    mov rax, SOB_TRUE
    jmp lt_B_end
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

(define plus-assembly
    (string-append  "\n;;   +   ;;\n" set-up-global-env
"
CL_plus:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_plus
    jmp L_plus
B_plus:
    push rbp
    mov rbp, rsp

    mov r9, qword [rbp + 4*8]
    TYPE r9,
    cmp r9, T_NIL
    je  plus_zero
    mov r9, qword [rbp + 4*8]
    mov rax, r9
    mov r14, qword [rbp + 3*8]
    sub r14, 1
    mov r15, 0
plus_loop:
    inc r15
    cmp r15, r14
    je  plus_end

    mov rbx, r15
    add rbx, 4
    shl rbx, 3
    mov r8, qword [rbp + rbx]   ; rbx = (r15 + 4)*8
    ; r9 = a/b, r8 = c/d
    NUMERATOR r9
    mov r10, rax        ; r10 = a
    DENOMINATOR r8
    mul r10             
    mov r10, rax        ; r10 = a*d
    NUMERATOR r8
    mov r11, rax        ; r11 = c
    DENOMINATOR r9
    mul r11
    mov r11, rax        ; r11 = b*c
    DENOMINATOR r8
    mov r12, rax
    DENOMINATOR r9
    mul r12
    mov r12, rax        ; r12 = b*d = solution denominator
    add r10, r11        ; r10 = solution numerator
    GCD r10, r12
    mov r11, rax        ; r11 = gcd(numerator, denominator)
    mov rax, r10
    div r11
    mov r13, rax        ; r13 = numerator after reduction
    mov rax, r12
    div r11
    mov r12, rax        ; r12 = denominator after reduction
    mov rdi, 8
    call malloc 
    mov r8, r13
    mov r13, rax
    mov qword [r13], r8
    mov rdi, 8
    call malloc 
    mov r9, r12
    mov r12, rax
    mov qword [r12], r9
    mov rdi, 8

    REDUCE_FRAC_TO_INT r10, r13, r12    ; r10 = pointer to dest, r8 - numerator, r9 - denominator

    mov r9, r10
    jmp plus_loop
plus_zero:
    mov rax, 0
    sal rax, TYPE_BITS
    or rax, T_INTEGER
plus_end:
    leave
    ret

L_plus:
    mov rax, qword [rax]
    mov qword [fvar_17], rax
    mov rax, SOB_VOID
"))

(define numerator-assembly
    (string-append  "\n;;   numerator   ;;\n" set-up-global-env
"
CL_numerator:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_numerator

    jmp L_numerator
B_numerator:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
    NUMERATOR rax
    sal rax, TYPE_BITS
    or  rax, T_INTEGER
    leave
    ret
L_numerator:
    mov rax, qword [rax]
    mov qword [fvar_18], rax
    mov rax, SOB_VOID
"))

(define denominator-assembly
    (string-append  "\n;;   denominator   ;;\n" set-up-global-env
"
CL_denominator:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_denominator

    jmp L_denominator
B_denominator:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
    DENOMINATOR rax
    sal rax, TYPE_BITS
    or  rax, T_INTEGER
    leave
    ret
L_denominator:
    mov rax, qword [rax]
    mov qword [fvar_19], rax
    mov rax, SOB_VOID
"))

(define mul-assembly
    (string-append  "\n;;   mul   ;;\n" set-up-global-env
"
CL_mul:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_mul
    jmp L_mul
B_mul:
    push rbp
    mov rbp, rsp

    mov r9, qword [rbp + 4*8]
    TYPE r9,
    cmp r9, T_NIL
    je  mul_one
    mov r9, qword [rbp + 4*8]
    mov rax, r9
    mov r14, qword [rbp + 3*8]
    sub r14, 1
    mov r15, 0
mul_loop:
    inc r15
    cmp r15, r14
    je  mul_end

    mov rbx, r15
    add rbx, 4
    shl rbx, 3
    mov r8, qword [rbp + rbx]   ; rbx = (r15 + 4)*8
    ; r9 = a/b, r8 = c/d
    NUMERATOR r9
    mov r10, rax        ; r10 = a
    NUMERATOR r8
    mul r10             
    mov r10, rax        ; r10 = a*c = solution numerator
    DENOMINATOR r8
    mov r11, rax        ; r11 = b
    DENOMINATOR r9
    mul r11
    mov r11, rax        ; r11 = b*d = solution denominator
    GCD r10, r11
    mov r12, rax        ; r12 = gcd(numerator, denominator)
    mov rax, r10
    div r12
    mov r13, rax        ; r13 = numerator after reduction
    mov rax, r11
    div r12
    mov r12, rax        ; r12 = denominator after reduction
    mov rdi, 8
    call malloc 
    mov r8, r13
    mov r13, rax
    mov qword [r13], r8
    mov rdi, 8
    call malloc 
    mov r9, r12
    mov r12, rax
    mov qword [r12], r9
    mov rdi, 8

    REDUCE_FRAC_TO_INT r10, r13, r12    ; r10 = pointer to dest, r8 - numerator, r9 - denominator

    mov r9, r10
    jmp mul_loop
mul_one:
    mov rax, 1
    sal rax, TYPE_BITS
    or  rax, T_INTEGER
mul_end:
    leave
    ret

L_mul:
    mov rax, qword [rax]
    mov qword [fvar_21], rax
    mov rax, SOB_VOID
"))

(define div-assembly
    (string-append  "\n;;   /   ;;\n" set-up-global-env
"
CL_div:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_div
    jmp L_div
B_div:
    push rbp
    mov rbp, rsp

    mov r9, qword [rbp + 4*8]
    TYPE r9,
    cmp r9, T_NIL
    je  END                     ; Dividing with no parameters is invalid
    mov r14, qword [rbp + 3*8]
    cmp r14, 3
    jl  div_with_1
    mov r9, qword [rbp + 4*8]
    mov rax, r9
    mov r15, 1
    jmp div_loop
div_with_1:
    mov rax, 1
    sal rax, TYPE_BITS
    or  rax, T_INTEGER
    mov r9, rax
    mov r15, 0
div_loop:
    inc r15
    cmp r15, r14
    je  div_end
    mov rbx, r15
    add rbx, 3
    shl rbx, 3
    mov r8, qword [rbp + rbx]   ; rbx = (r15 + 3)*8
    ; r9 = a/b, r8 = c/d
    NUMERATOR r9
    mov r10, rax        ; r10 = a
    DENOMINATOR r8
    mul r10             
    mov r10, rax        ; r10 = a*d = solution numerator
    DENOMINATOR r9
    mov r11, rax        ; r11 = b
    NUMERATOR r8
    mul r11
    mov r11, rax        ; r11 = b*c = solution denominator
    cmp r11, 0
    GCD r10, r11
    mov r12, rax        ; r12 = gcd(numerator, denominator)
    mov rax, r10
    div r12
    mov r13, rax        ; r13 = numerator after reduction
    mov rax, r11
    div r12
    mov r12, rax        ; r12 = denominator after reduction
    mov rdi, 8
    call malloc 
    mov r8, r13
    mov r13, rax
    mov qword [r13], r8
    mov rdi, 8
    call malloc 
    mov r9, r12
    mov r12, rax
    mov qword [r12], r9
    mov rdi, 8

    REDUCE_FRAC_TO_INT r10, r13, r12    ; r10 = pointer to dest, r8 - numerator, r9 - denominator

    mov r9, r10
    jmp div_loop    
div_end:
    ; check that value is not integer
    mov r15, rax
    TYPE r15
    xor r15,T_FRACTION
    cmp r15,0
    jne ENDDivide
    ; if Denom is negative, multiply Denom and Numer by -1
    mov r15, rax
    DENOMINATOR r15
    cmp rax,0
    jge DenomWasPositive

    ; denom was negative, need to multiply
    mov r13, rax
    CAR rax
    mov r15, rax
    mov rax,-1
    mov r14, qword [r15]
    mul r14
    mov qword [r15], r14
    mov rax, r13
    
    CDR rax
    mov r15, rax
    mov rax,-1
    mov r14, qword [r15]
    mul r14
    mov qword [r15], r14
    mov rax, r13
    
    jmp ENDDivide
    
DenomWasPositive:
    mov rax, r15
    
ENDDivide:    
    leave
    ret

L_div:
    mov rax, qword [rax]
    mov qword [fvar_22], rax
    mov rax, SOB_VOID
"))

(define subtract-assembly
    (string-append  "\n;;   -   ;;\n" set-up-global-env
"
CL_sub:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_sub
    jmp L_sub
B_sub:
    push rbp
    mov rbp, rsp
    mov r9, qword [rbp + 4*8]
    TYPE r9
    cmp r9, T_NIL
    je  END                     ; Subtracting with no parameters is invalid
    mov r14, qword [rbp + 3*8]
    cmp r14, 3
    jl  sub_with_0
    mov r9, qword [rbp + 4*8]
    mov rax, r9
    mov r15, 1
    jmp sub_loop
sub_with_0:
    mov rax, 0
    sal rax, TYPE_BITS
    or  rax, T_INTEGER
    mov r9, rax
    mov r15, 0
sub_loop:
    inc r15
    cmp r15, r14
    je  sub_end
    mov rbx, r15
    add rbx, 3
    shl rbx, 3
    mov r8, qword [rbp + rbx]   ; rbx = (r15 + 3)*8
    ; r9 = a/b, r8 = c/d
    NUMERATOR r9
    mov r10, rax        ; r10 = a
    DENOMINATOR r8
    mul r10             
    mov r10, rax        ; r10 = a*d
    NUMERATOR r8
    mov r11, rax        ; r11 = c
    DENOMINATOR r9
    mul r11
    mov r11, rax        ; r11 = b*c
    DENOMINATOR r8
    mov r12, rax
    DENOMINATOR r9
    mul r12
    mov r12, rax        ; r12 = b*d = solution denominator
    sub r10, r11        ; r10 = solution numerator
    GCD r10, r12
    mov r11, rax        ; r11 = gcd(numerator, denominator)
    mov rax, r10
    div r11
    mov r13, rax        ; r13 = numerator after reduction
    mov rax, r12
    div r11
    mov r12, rax        ; r12 = denominator after reduction
    mov rdi, 8
    call malloc 
    mov r8, r13
    mov r13, rax
    mov qword [r13], r8
    mov rdi, 8
    call malloc 
    mov r9, r12
    mov r12, rax
    mov qword [r12], r9
    mov rdi, 8

    REDUCE_FRAC_TO_INT r10, r13, r12    ; r10 = pointer to dest, r8 - numerator, r9 - denominator

    mov r9, r10
    jmp sub_loop
sub_end:
    leave
    ret

L_sub:
    mov rax, qword [rax]
    mov qword [fvar_23], rax
    mov rax, SOB_VOID
")) 

(define remainder-assembly
    (string-append  "\n;;   remainder   ;;\n" set-up-global-env
"
CL_remainder:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_remainder

    jmp L_remainder
B_remainder:
    push rbp
    mov rbp, rsp

    mov r14, qword [rbp + 3*8]
    cmp r14, 3
    jne END
    
    mov r12, qword [rbp + 4*8]
    DATA r12
    mov r13, qword [rbp + 5*8]
    DATA r13
    ; r12 = to-divide (integer), r13 = divisor (integer)
    mov rax, r12
    mov rbx, r13
    mov rdx, 0
    div rbx
    sal rdx, TYPE_BITS
    or  rdx, T_INTEGER
    mov rax, rdx
    leave
    ret

L_remainder:
    mov rax, qword [rax]
    mov qword [fvar_24], rax
    mov rax, SOB_VOID
"))

(define char->integer-assmebly
    (string-append "\n;;    char->integer   ;;\n" set-up-global-env
"
CL_char_integer:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_char_integer

    jmp L_char_integer
B_char_integer:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
    mov rbx, T_CHAR
    xor rax, rbx
    mov rbx, T_INTEGER
    xor rax, rbx
    leave
    ret
L_char_integer:
    mov rax, qword [rax]
    mov qword [fvar_25], rax
    mov rax, SOB_VOID
"))

(define cons-assembly
    (string-append "\n;;    cons   ;;\n" set-up-global-env
"
CL_cons:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_cons

    jmp L_cons
B_cons:
    push rbp
    mov rbp, rsp
    mov r12, qword [rbp + 4*8]
    mov r13, qword [rbp + 5*8]
    mov rdi, 8
    call malloc 
    mov qword[rax], r12
    mov r12, rax
    mov rdi, 8
    call malloc 
    mov qword[rax], r13
    mov r13, rax
    mov rdi, 8
    call malloc 
    MAKE_MALLOC_LITERAL_PAIR rax, r12, r13  
    mov rax, qword [rax]
    leave
    ret
L_cons:
    mov rax, qword [rax]
    mov qword [fvar_27], rax
    mov rax, SOB_VOID
"))

(define list-assembly
    (string-append "\n;;;     list     ;;;\n" set-up-global-env
"
CL_list:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_list

    jmp L_list
B_list:
    push rbp
    mov rbp, rsp
    mov r8, qword [rbp + 3*8]
    cmp r8, 1
    je  B_list_empty
    mov r9, r8
    mov r10, 0
B_list_loop:
    cmp r10, r9
    je  B_list_end
    mov r14, r8
    add r14, 3
    sal r14, 3              
    add r14, rbp            
    mov r13, qword [r14]    ; r14 = rbp + (i+4)*8
    mov rdi, 8
    call malloc 
    mov qword[rax], r13
    mov r13, rax

    sub r14, 8              
    mov r12, qword [r14]    ; r14 = rbp + (i+3)*8
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
    jmp B_list_loop
B_list_empty:
    mov rax, SOB_NIL
    jmp B_list_finish
B_list_end:
    mov rax, qword [rbp + 4*8]
B_list_finish:
    leave
    ret
L_list:
    mov rax, qword [rax]
    mov qword [fvar_28], rax
    mov rax, SOB_VOID
"))

(define make-vector-assembly
    (string-append "\n;;;     make-vector     ;;;\n" set-up-global-env
"
CL_make_vector:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_make_vector

    jmp L_make_vector
B_make_vector:
    push rbp
    mov rbp, rsp
    mov r12, qword [rbp + 4*8]
    DATA r12
    mov r15, 0
    sal r15, TYPE_BITS
    or  r15, T_INTEGER
    mov rdi, 8
    call malloc 
    mov qword[rax], r15
    mov r15, rax
    mov rdi, r12
    shl rdi, 3
    call malloc
    mov rbx, rax
    mov rdx, 0

B_make_vector_elements_loop:
    cmp rdx, r12
    je  make_vector_register
    mov qword[rbx], r15
    add rbx, 8
    inc rdx
    jmp B_make_vector_elements_loop
make_vector_register:
    mov r13, r12
    sal r13, ((WORD_SIZE - TYPE_BITS) >> 1)
    sub rax, start_of_data
    add r13, rax
    sal r13, TYPE_BITS
    or  r13, T_VECTOR
    mov rax, r13
    leave
    ret
L_make_vector:
    mov rax, qword [rax]
    mov qword [fvar_29], rax
    mov rax, SOB_VOID
"))

(define vector-assembly
    (string-append "\n;;;     vector     ;;;\n" set-up-global-env
"
CL_vector_create:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_vector_create
    jmp L_vector_create
B_vector_create:
    push rbp
    mov rbp, rsp
    mov r12, qword [rbp + 3*8]
    sub r12, 1
    mov rdi, r12
    shl rdi, 3
    call malloc
    mov r14, rax
    mov rbx, rax
    mov rdx, 0
B_vector_elements_loop:
    cmp rdx, r12
    je  vector_register
    push rbx
    push rdx
    mov rdi, 8
    call malloc 
    pop rdx
    mov r15, qword [rbp + (4 + rdx)*8]
    mov qword[rax], r15
    mov r15, rax
    pop rbx
    mov qword[rbx], r15
    add rbx, 8
    inc rdx
    jmp B_vector_elements_loop
vector_register:
    mov r13, r12
    sal r13, ((WORD_SIZE - TYPE_BITS) >> 1)
    sub r14, start_of_data
    add r13, r14
    sal r13, TYPE_BITS
    or  r13, T_VECTOR
    mov rax, r13
    leave
    ret
L_vector_create:
    mov rax, qword [rax]
    mov qword [fvar_30], rax
    mov rax, SOB_VOID
"))

(define vector-length-assembly
    (string-append "\n;;;     vector-length     ;;;\n" set-up-global-env
"
CL_vector_length:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_vector_length
    jmp L_vector_length
B_vector_length:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
    VECTOR_LENGTH rax
    shl rax, TYPE_BITS
    or  rax, T_INTEGER
    leave
    ret
L_vector_length:
    mov rax, qword [rax]
    mov qword [fvar_31], rax
    mov rax, SOB_VOID
"))

(define vector-ref-assembly
    (string-append "\n;;;     vector-ref     ;;;\n" set-up-global-env
"
CL_vector_ref:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_vector_ref

    jmp L_vector_ref
B_vector_ref:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
    mov r13, qword [rbp + 5*8]
    DATA r13
    sal r13, 3
    VECTOR_ELEMENTS rax
    add rax, r13
    mov rax, qword [rax]
    push qword [rax]
    call write_sob_if_not_void
    add rsp, 8
    leave
    ret
L_vector_ref:
    mov rax, qword [rax]
    mov qword [fvar_32], rax
    mov rax, SOB_VOID
"))

(define vector-set!-assembly
    (string-append "\n;;;     vector-set     ;;;\n" set-up-global-env
"
CL_vector_set:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_vector_set

    jmp L_vector_set
B_vector_set:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]  ; vector
    mov r13, qword [rbp + 5*8]  ; index
    mov r14, qword [rbp + 6*8]  ; to-add
    DATA r13
    sal r13, 3
    VECTOR_ELEMENTS rax
    add rax, r13
    mov rax, qword [rax]
    mov qword [rax], r14
    mov rax, SOB_VOID
    add rsp, 8
    leave
    ret
L_vector_set:
    mov rax, qword [rax]
    mov qword [fvar_33], rax
    mov rax, SOB_VOID
"))

(define make-string-assembly
    (string-append "\n;;;     make-string     ;;;\n" set-up-global-env
"
CL_make_string:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_make_string

    jmp L_make_string
B_make_string:
    push rbp
    mov rbp, rsp
    mov r13, qword [rbp + 4*8]
    DATA r13
    mov rdi, r13
    sal rdi, 3
    call malloc
    mov rbx, 0
B_make_string_loop:
    cmp rbx, r13
    je make_string_register
    mov qword[rax + rbx], 0
    inc rbx
    jmp B_make_string_loop
make_string_register:
    sal r13, ((WORD_SIZE - TYPE_BITS) >> 1)
    sub rax, start_of_data
    add r13, rax
    sal r13, TYPE_BITS
    or  r13, T_STRING
    mov rax, r13
    leave
    ret
L_make_string:
    mov rax, qword [rax]
    mov qword [fvar_34], rax
    mov rax, SOB_VOID
"))

(define string-length-assembly
    (string-append "\n;;;     string-length     ;;;\n" set-up-global-env
"
CL_string_length:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_string_length

    jmp L_string_length
B_string_length:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
    STRING_LENGTH rax
    sal rax, 4
    or rax, T_INTEGER
    leave
    ret
L_string_length:
    mov rax, qword [rax]
    mov qword [fvar_35], rax
    mov rax, SOB_VOID
"))

(define string-ref-assembly
    (string-append "\n;;;     string-ref     ;;;\n" set-up-global-env
"
CL_string_ref:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_string_ref

    jmp L_string_ref
B_string_ref:
    push rbp
    mov rbp, rsp
    mov rbx, qword [rbp + 4*8]
    mov rcx, qword [rbp + 5*8]
    DATA rcx                    ; offset
    STRING_ELEMENTS rbx
    add rbx, rcx
    movzx rax, byte [rbx]
    shl rax, TYPE_BITS
    or rax, T_CHAR
    leave
    ret
L_string_ref:
    mov rax, qword [rax]
    mov qword [fvar_36], rax
    mov rax, SOB_VOID
"))

(define string-set!-assembly
    (string-append "\n;;;     string-set     ;;;\n" set-up-global-env
"
CL_string_set:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_string_set

    jmp L_string_set
B_string_set:
    push rbp
    mov rbp, rsp
    mov rbx, qword [rbp + 4*8]
    mov rcx, qword [rbp + 5*8]
    DATA rcx                    ; offset
    STRING_ELEMENTS rbx
    add rbx, rcx
    mov rdx, qword [rbp + 6*8]
    DATA rdx
    mov byte [rbx], dl
    mov rax, SOB_VOID
    leave
    ret
L_string_set:
    mov rax, qword [rax]
    mov qword [fvar_37], rax
    mov rax, SOB_VOID
"))

(define symbol->string-assembly
    (string-append "\n;;;     symbol->string     ;;;\n" set-up-global-env
"
CL_symbol_string:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_symbol_string

    jmp L_symbol_string
B_symbol_string:
    push rbp
    mov rbp, rsp
    mov r15, qword [rbp + 4*8]
    push SOB_TRUE
    call write_sob_if_not_void
    add rsp, 8
    SYMBOL_STRING r15
    leave
    ret
L_symbol_string:
    mov rax, qword [rax]
    mov qword [fvar_38], rax
    mov rax, SOB_VOID
"))

(define eq?-assembly
    (string-append "\n;;;     eq?     ;;;\n" set-up-global-env
"
CL_eq:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_eq

    jmp L_eq
B_eq:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
    mov rbx, qword [rbp + 5*8]
    
    ;at this point we have both items in the registers
    
    and rax, 15
    and rbx, 15
    cmp rax, rbx
    jne B_eq_false
    
    ; now we know they are the same type
    ; if nil - always equal
    cmp rax, T_NIL
    je B_eq_true
    
    ; pair,vector,string check both L&U, others only check Lower
    
    cmp rax, T_VECTOR
    je CheckBothInEQ
    cmp rax, T_PAIR
    je CheckBothInEQ
    cmp rax, T_STRING
    je CheckBothInEQ
    
    ; only check for data lower
    jmp CheckLowerInEQ
    
CheckBothInEQ:
        mov rax, qword [rbp + 4*8]
    mov rbx, qword [rbp + 5*8]

    cmp rax, rbx
    jne B_eq_false
    jmp B_eq_true   


CheckLowerInEQ:
        ; load them again
    mov rax, qword [rbp + 4*8]
    mov rbx, qword [rbp + 5*8]
    
    DATA_LOWER rax
    DATA_LOWER rbx
    
    cmp rax, rbx
    jne B_eq_false
    jmp B_eq_true       
    
    

B_eq_false:
    mov rax, SOB_FALSE
    jmp B_eq_end
B_eq_true:
    mov rax, SOB_TRUE
    jmp B_eq_end
        
B_eq_end:
    leave
    ret
L_eq:
    mov rax, qword [rax]
    mov qword [fvar_39], rax
    mov rax, SOB_VOID
"))

(define string->symbol-assembly
    (string-append "\n;;;    string-> symbol ;;; \n" set-up-global-env
    "
CL_string_symbol:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_string_symbol

    jmp L_string_symbol
B_string_symbol:
    push rbp
    mov rbp, rsp
    mov r15, qword [rbp + 4*8]    ; pointer to a string
    mov r14, symbol_table
    mov rax, qword [r14]        ; rax is new node
B_string_symbol_loop:
    DATA_LOWER rax
    cmp rax, 0
    je  B_string_symbol_not_found

    add rax, start_of_data
    mov r14, rax
    mov rax, qword [rax]
    mov rbx, rax
    DATA_UPPER rbx
    add rbx, start_of_data
    mov rbx, qword [rbx]            ; rbx is symbol
    mov rcx, rbx
    mov r9, rax
    SYMBOL_STRING rcx
    mov rcx, rax
    mov rax, r9
    cmp rcx, r15
    je  B_string_symbol_found
    jmp B_string_symbol_loop
B_string_symbol_found:
    mov rax, rbx
    jmp B_string_symbol_end
B_string_symbol_not_found:
    ; r14 - pointer to last node, r15 - new symbol string 
    mov rdi, 8          ; create new symbol
    call malloc
    mov qword [rax], r15
    sub rax, start_of_data
    sal rax, TYPE_BITS
    or  rax, T_SYMBOL
    mov r13, rax            ; r13 = symbol
    push rax
    mov rdi, 8
    call malloc 
    mov qword [rax], r13
    mov r13, rax
    sub r13, start_of_data  ; r13 = relative address of symbol
    sal r13, 34
    mov rdi, 8          ; create new node
    call malloc
    mov qword [rax], r13
    mov r12, rax            ; r12 - pointer to new node
    sub r12, start_of_data  
    mov r13, qword [r14]
    sar r13, TYPE_BITS
    add r13, r12
    sal r13, TYPE_BITS
    mov qword [r14], r13
    pop rax                 ; return the symbol register
B_string_symbol_end:
    leave
    ret
L_string_symbol:
    mov rax, qword [rax]
    mov qword [fvar_40], rax
    mov rax, SOB_VOID
"))

(define apply-assembly
    (string-append "\n;;; append ;;; \n" set-up-global-env
    "
CL_apply:
    mov rdi, 16
    call malloc 
    MAKE_LITERAL_CLOSURE rax, rbx, B_apply

    jmp L_apply
B_apply:
    push rbp
    mov rbp, rsp
    mov rax, qword [rbp + 4*8]
    mov rbx, qword [rbp + 5*8]
    mov r8, 0
B_apply_loop:
    mov rcx, rbx
    cmp rcx, SOB_NIL
    je  B_apply_call
    CAR rcx
    push rcx
    CDR rbx
    inc r8
    jmp B_apply_loop
B_apply_call:
    push rcx
    inc r8
    push r8
    
    mov rbx, rax            ; apply the closure
    CLOSURE_ENV rbx
    CLOSURE_CODE rax
    push rbx
    call rax
    sal r8, 3
    add r8, 8
    add rsp, r8
    leave
    ret
L_apply:
    mov rax, qword [rax]
    mov qword [fvar_41], rax
    mov rax, SOB_VOID
"))

(define get-runtime-assembly-functions
    (lambda ()
        (string-append
            "\n;; Start of library functions \n\n"
            not-assembly
            boolean?-assembly
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
            plus-assembly
            numerator-assembly
            denominator-assembly
            mul-assembly
            div-assembly
            subtract-assembly
            remainder-assembly
            char->integer-assmebly
            cons-assembly
            list-assembly
            make-vector-assembly
            vector-assembly
            vector-length-assembly
            vector-ref-assembly
            vector-set!-assembly
            make-string-assembly
            string-length-assembly
            string-ref-assembly
            string-set!-assembly
            symbol->string-assembly
            eq?-assembly
            string->symbol-assembly
            apply-assembly
            "\n;; End of assembly library functions \n\n")))


(define get-scheme-impls
    '((define integer->char char->integer)
    (define rational? number?)
    (define zero? (lambda (x) (and (number? x) (= 0 x))))
    (define map (lambda (f l) (if (null? l) '() (cons (f (car l)) (map f (cdr l))))))
    (define regular-append
        (lambda (l m)
            (if (null? l)
                m
                (cons (car l) (regular-append (cdr l) m)))))
             
    (define append-helper
        (lambda (x y)
            (if (null? y)
                x
                (regular-append x (append-helper (car y) (cdr y))))))

    (define append
        (lambda lst
            (cond 
                ((null? lst) '())
                ((null? (cdr lst)) (car lst))
                ((= 2 (cdr (cdr lst))) (regular-append (car lst) (car (cdr lst))))
                (else 
                    (append-helper (car lst) (cdr lst))))))))

