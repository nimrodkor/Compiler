
(define scheme-to-simple-scheme-procs
"
apply
<
>
=
+
/
*
-
car
cdr
char->integer
char?
cons
denominator
eq?
integer?
integer->char
list
make-string
make-vector
map
null?
number?
numerator
pair?
procedure?
rational?
remainder
set-car!
set-cdr!
string-length
string-ref
string-set!
string->symbol
string?
symbol?
symbol->string
vector
vector-length
vector-ref
vector-set!
vector?

(define zero? (lambda (x) (= 0 x)))
(define not (lambda (x) (if (eq? x #f) #t #f)))
(define map (lambda ( func lst) (if (null? lst) '() (cons (func (car lst)) (map func (cdr lst))))))

(define map
    (lambda ( func lst)
    (if (null? lst)
        '()
        (cons (func (car lst)) (map func (cdr lst))))))
        
(define length
    (lambda (lst)
        (if (null? lst)
            0
            (+ 1 (length (cdr lst))))))
            
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
	            (append-helper (car lst) (cdr lst))))))
")

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

