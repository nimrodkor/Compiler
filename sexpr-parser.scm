(load "pc.scm")

;;;;;;;; Helper methods ;;;;;;;;;

(define <whitespace>
  (const (lambda (ch) (char<=? ch #\space))))

(define <digit-0-9> (range #\0 #\9))

(define <letter-A-Z> (range #\A #\Z))
        
(define <letter-a-z> (range #\a #\z))

(define <HexChar>
    (new 
        (*parser (range #\0 #\9))
        (*pack
            (lambda (ch) (- (char->integer ch) (char->integer #\0))))
        (*parser (range #\a #\f))
        (*pack
            (lambda (ch) (+ 10 (- (char->integer ch) (char->integer #\a)))))
        (*parser (range #\A #\F))
        (*pack
            (lambda (ch) (+ 10 (- (char->integer ch) (char->integer #\A)))))
        (*disj 3)
     done))

(define power
    (lambda (x e) (if (= 0 e) 1 (* x (power x (- e 1))))))

(define hex->integer
    (lambda (hex-digit-list to-the-power-of)
        (if (null? hex-digit-list)
            0
            (+ 
                (* (car hex-digit-list) (power 16 to-the-power-of)) 
                (hex->integer (cdr hex-digit-list) (- to-the-power-of 1)))
        )))

(define word-to-char-converter
    (lambda (str char)
        (new (*parser (word str))
            (*pack (lambda (_) char))
        done)))


(define <end-of-line>
    (new
        (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
    done))


(define <LineComments>
    (new
        (*parser (char #\;))

        (*parser <any-char>)
        (*parser <end-of-line>)
        *diff *star

        (*parser <end-of-line>)
        (*caten 3)
    done))


(define <SexprComments>
    (new
        (*parser (word "#;"))
        (*delayed (lambda () <sexpr>))
        (*caten 2)
    done))

(define <SexprIgnore>
    (new
        (*parser <SexprComments>)
        (*parser <whitespace>)
        (*parser <LineComments>)
        (*disj 3)
    done))


;;;;;;;;;;;;;Important stuff from here ;;;;;;;;;;;;;
;; Boolean

(define <Boolean>
    (new 
        (*parser (word-ci "#t"))
        (*pack (lambda (_) #t))
        (*parser (word-ci "#f"))
        (*pack (lambda (_) #f))       
        (*disj 2)
    done))

;; Char

(define <CharPrefix>
	(new 
        (*parser (word "#\\"))
    done))

(define <NamedChar>
    (new 
        (*parser (word-ci "lambda"))
        (*pack (lambda (x) #\λ))
        (*parser (word-ci "newline"))
        (*pack (lambda (x) #\newline)) 
        (*parser (word-ci "nul"))
        (*pack (lambda (x) #\nul))
        (*parser (word-ci "page"))
        (*pack (lambda (x) #\page))
        (*parser (word-ci "return"))
        (*pack (lambda (x) #\return))
        (*parser (word-ci "space"))
        (*pack (lambda (x) #\space)) 
        (*parser (word-ci "tab"))
        (*pack (lambda (x) #\tab))
        (*disj 7)
    done))

(define <VisibleSimpleChar>
    (new 
        (*parser (range  #\! #\~))
        (*parser (char #\x))
        *diff
        (*pack 
            (lambda (ch) ch))
    done))

(define <HexUnicodeChar>
    (new
        (*parser (char #\x))
        (*parser <HexChar>) *plus
        (*caten 2)
        (*pack-with
            (lambda (x num)
                (integer->char (hex->integer num (- (length num) 1)))))
       done))

(define <Char>
    (new 
        (*parser <CharPrefix>)

        (*parser <NamedChar>)
        (*parser <VisibleSimpleChar>)
        (*parser <HexUnicodeChar>)
        (*disj 3)
        (*parser <letter-a-z>)
        (*parser <letter-A-Z>)
        (*parser <digit-0-9>)
        (*disj 3)
        *not-followed-by

        (*caten 2)
        (*pack-with (lambda (a b) b))
    done))

;; Number

(define <Sign>
    (new
        (*parser (char #\+))
        (*pack
            (lambda (ch) +))
        (*parser (char #\-))
        (*pack 
            (lambda (ch) -))
        (*disj 2)
    done))

(define <Natural>
    (new
        (*parser (range #\0 #\9))*plus
        (*pack
            (lambda (list-num) (string->number (list->string list-num))))
    done))

(define <Integer>
    (new
        (*parser <Sign>)
        (*parser <Natural>)
        (*caten 2)
        (*pack-with 
            (lambda (sign num) (sign num)))
                
        (*parser <Natural>)
        (*disj 2)
    done))

(define <Fraction>
    (new 
        (*parser <Integer>)
        (*parser (char #\/))
        (*parser <Natural>)
        (*caten 3)
        (*pack-with
            (lambda (mone div mehane)
                (/ mone mehane)))
    done))

(define <Number>
    (new
        (*parser <Integer>)
        (*parser (char #\/))
        *not-followed-by
        (*parser <Fraction>)
        (*disj 2)
    done))

;; String

(define <StringLiteralChar>
    (new
        (*parser <any-char>)
        (*parser (char #\\))
        *diff
    done))

(define <StringMetaChar>
    (new
        (*parser (word-to-char-converter "\\\\" #\\))
        (*parser (word-to-char-converter "\\\"" #\"))
        (*parser (word-to-char-converter "\\t" #\tab))
        (*parser (word-to-char-converter "\\f" #\page))
        (*parser (word-to-char-converter "\\n" #\newline))
        (*parser (word-to-char-converter "\\r" #\return))
        (*disj 6)
    done))

(define <StringHexChar>
    (new
        (*parser (char #\\))
        (*parser (char #\x))
        (*parser <HexChar>)*star
        (*pack
            (lambda (hex-digit-list)
                (hex->integer hex-digit-list (- (length hex-digit-list) 1))))
        (*parser (char #\;))
        (*caten 4)
        (*pack-with
            (lambda (backslach x hexchar semicolon) (integer->char hexchar)))
    done))

(define <StringChar>
    (new
        (*parser <StringLiteralChar>)
        (*parser <StringMetaChar>)
        (*parser <StringHexChar>)
        (*disj 3)
    done))

(define <String>
    (new
        (*parser (char #\"))

        (*parser <StringChar>)
        (*parser (char #\"))
        *diff
        *star

        (*parser (char #\"))
        
        (*caten 3)
        (*pack-with
            (lambda (start string-list end)
                (list->string string-list)))
    done))

;; Symbol

(define <SymbolChar>
    (new 
        (*parser (range #\0 #\9))
        (*parser <letter-A-Z>)
        (*pack (lambda (uppercase) 
                (integer->char (+ (char->integer uppercase) 32))))
        (*parser <letter-a-z>)
        (*parser (char #\!))
        (*parser (char #\$))
        (*parser (char #\^))
        (*parser (char #\*))
        (*parser (char #\-))
        (*parser (char #\_))
        (*parser (char #\=))
        (*parser (char #\+))
        (*parser (char #\>))
        (*parser (char #\<))
        (*parser (char #\?))
        (*parser (char #\/))
        (*disj 15)
    done))

(define <Symbol>
    (new
        (*parser <SymbolChar>)*plus
        (*pack (lambda (ch) (string->symbol (list->string ch))))
    done))

;; ProperList

(define <ProperList>
    (new
        (*parser (char #\( ))
        (*delayed (lambda () <sexpr>))*star
        (*parser (char #\) ))
        (*caten 3)
        (*pack-with (lambda (parent value hasis) value))
    done))

;; ImproperList

(define <ImproperList>
    (new
        (*parser (char #\())
        (*delayed (lambda () <sexpr>)) *plus
        (*parser (char #\.))
        (*delayed (lambda () <sexpr>))
        (*parser (char #\)))
        (*caten 5)
        (*pack-with
            (lambda (parent value-list dot value hasis)
                `(,@value-list . ,value)))
    done))

;; Vector

(define <Vector>
    (new
        (*parser (char #\#))
        (*parser (char #\())
        (*delayed (lambda () <sexpr>))*star
        (*parser (char #\)))
        (*caten 4)
        (*pack-with
            (lambda (hashtag parent value-list hasis)
                (list->vector `(,@value-list))))
    done))

;; Quotes, Quasi-Quotes and so on

(define <Quoted>
    (new
        (*parser (char #\' ))
        (*delayed (lambda () <sexpr>))
        (*caten 2)
        (*pack-with
            (lambda (q value) (list 'quote value)))
    done))

(define <QuasiQuoted>
    (new
        (*parser (char #\`))
        (*delayed (lambda () <sexpr>))
        (*caten 2)
        (*pack-with
            (lambda (q value) (list 'quasiquote value)))
    done))

(define <UnQuoted>
    (new
        (*parser (char #\,))
        (*delayed (lambda () <sexpr>))
        (*caten 2)
        (*pack-with
            (lambda (u value) (list 'unquote value)))
    done))

(define <UnQuotedAndSpliced>
    (new
        (*parser (word ",@"))
        (*delayed (lambda () <sexpr>))
        (*caten 2)
        (*pack-with
            (lambda (unq-spl value) (list 'unquote-splicing value)))
    done))

;; CBName

(define <CBName1>
    (new
        (*parser (char #\@))
        (*delayed (lambda () <sexpr>))
        (*caten 2)
        (*pack-with
            (lambda (cbchar value) (list 'cbname value)))
    done))

(define <CBName2>
    (new
        (*parser (char #\{))
        (*delayed (lambda () <sexpr>))
        (*parser (char #\}))
        (*caten 3)
        (*pack-with
            (lambda (open value close) (list 'cbname value)))
    done))

(define <CBName>
    (new
        (*parser <CBName1>)
        (*parser <CBName2>)
        (*disj 2)
    done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Infix ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Infix helpers
;; Helper for handling cases with more than one operation of same ordering.
;; Examples:
;; 2 + 3 + 4 - 6 - 12 + 5
;; 2 * 3 / 2 * 4 / 5 
(define handle-multi-operators
    (lambda (computed to-be-added)
        (if (null? to-be-added)
            computed
            (handle-multi-operators 
                (list (caar to-be-added) computed (cadar to-be-added))
                (cdr to-be-added)))))

(define handle-multi-powers
    (lambda (computed to-be-added)
        (if (null? to-be-added)
            computed
            (handle-multi-powers (list (car computed) (cadr computed)
                    (list (caar to-be-added) (caddr computed) (cadar to-be-added)))
                (cdr to-be-added)))))

(define <InfixIgnore>
    (new
        (*parser <SexprComments>)
        (*parser <whitespace>)
        (*parser <LineComments>)
        (*disj 3)
    done))


;; Helper for finding the next operator in cases of more than one operator 
;; of same ordering. 
(define find-next-operator
    (lambda (op1-as-char op1-to-return op2-as-char op2-to-return)
        (new
            (*parser <InfixIgnore>)*star

            (*parser (char op1-as-char))
            (*pack (lambda (ch) op1-to-return))
            (*parser (char op2-as-char))
            (*pack (lambda (ch) op2-to-return))
            (*disj 2)

            (*parser <InfixIgnore>)*star
            (*caten 3)
            (*pack-with
                (lambda (space1 op space2) op))
        done)))

;; Handles parsing of the arguments for InfixArgsList
(define func-argument-helper
    (lambda (computed-arguments other-arguments)
        (if (pair? other-arguments)
            (append (list computed-arguments) 
                (func-argument-helper 
                    (car other-arguments) (cdr other-arguments)))
            (list computed-arguments))))

;; Infix

(define <InfixPrefixExtensionPrefix>
    (new
        (*parser (word "##"))
        (*parser (word "#%"))
        (*disj 2)
    done))

(define <InfixSymbolChar>
    (new 
        (*parser (range #\0 #\9))
        (*parser <letter-A-Z>)
        (*pack (lambda (uppercase) 
                (integer->char (+ (char->integer uppercase) 32))))
        (*parser <letter-a-z>)
        (*parser (char #\!))
        (*parser (char #\$))
        (*parser (char #\_))
        (*parser (char #\=))
        (*parser (char #\>))
        (*parser (char #\<))
        (*parser (char #\?))
        (*disj 10)
        (*parser (char #\^))
        (*parser (char #\*))
        (*parser (char #\-))
        (*parser (char #\+))
        (*parser (char #\/))
        (*disj 5)
        *diff
    done))

(define <InfixSymbol>
    (new
        (*parser <InfixSymbolChar>)*plus
        (*pack (lambda (ch) (string->symbol (list->string ch))))
    done))

(define <InfixPowerSymbol>
    (new
        (*parser <InfixIgnore>)*star
        (*parser (char #\^))
        (*parser (word "**"))
        (*disj 2)
        (*parser <InfixIgnore>)*star
        (*caten 3)
        (*pack-with
            (lambda (space1 op space2) 'expt))
    done))

;; Enables to switch back and forth between Infix and Prefix
(define <SexprEscape>
    (new
        (*parser <InfixPrefixExtensionPrefix>)
        (*delayed (lambda () <sexpr>))
        (*caten 2)
        (*pack-with (lambda (prefix exp) exp))
    done))

(define <InfixNumber>
    (new
        (*parser <Integer>)
        (*parser (char #\/))
        *not-followed-by
        (*parser <Fraction>)
        (*disj 2)
    done))

(define <AtomicExpression>
    (new
        (*parser <InfixNumber>)
        (*parser <InfixSymbol>)
        (*parser <SexprEscape>)
        (*disj 3)
    done))

;; Infix Parenthasis
(define <InfixParen>
    (new
        (*parser (char #\( ))
        (*delayed (lambda () <InfixExpression>))
        (*parser (char #\)))
        (*caten 3)
        (*pack-with
            (lambda (parent exp hasis) exp))

        (*parser <AtomicExpression>)
        (*pack (lambda (exp) exp))
        (*disj 2)
    done))

(define <InfixArrayGet>
    (new
        (*parser <InfixIgnore>)*star
        (*parser <InfixParen>)
        (*parser (char #\[ ))
        (*delayed (lambda () <InfixExpression>))
        (*parser (char #\] ))
        (*caten 5)
        (*pack-with
            (lambda (ignore vector parent index hasis)
                (list 'vector-ref vector index)))

        (*parser <InfixIgnore>)*star
        (*parser (char #\[ ))
        (*delayed (lambda () <InfixExpression>))
        (*parser (char #\] ))
        (*caten 4)
        (*pack-with
            (lambda (ignore parent val hasis) (list 'vector-ref val)))
        *star

        (*caten 2)
        (*pack-with
            (lambda (base-operation other-operations) 
                (handle-multi-operators base-operation other-operations)))

    done))

(define <InfixArgsList>
    (new 
        (*parser <InfixIgnore>) *star

        ; One argument        
        (*delayed (lambda () <InfixExpression>))
        
        ; Multiple arguments
        (*parser (char #\,))
        (*parser <InfixIgnore>)*star
        (*caten 2)
        (*delayed (lambda () <InfixExpression>)) 
        (*caten 2)
        (*pack-with (lambda (coma a) a))
        *star
        (*caten 2)
        (*pack-with
            (lambda (first-argument other-arguments)
                (func-argument-helper first-argument other-arguments)))

        ; Zero arguments
        (*parser <epsilon>)

        (*disj 2)
        (*parser <InfixIgnore>) *star
        (*caten 3)
        (*pack-with
            (lambda (ignore1 input ignore2) input))
    done))

(define <InfixFuncall>
    (new
        (*parser <AtomicExpression>)
        (*parser <InfixIgnore>)*star
        (*parser (char #\( ))
        (*parser <InfixArgsList>)
        (*parser (char #\)))
        (*caten 5)
        (*pack-with (lambda (symbol ignore parent args hasis)
            (if (pair? args)
                (append (list symbol) args)
                (if (null? args)
                    (list symbol)
                    (list symbol args)))))
    done))

(define <InfixNeg>
    (new
        (*parser <InfixIgnore>)*star
        (*parser (char #\-))
        (*parser <InfixIgnore>)*star
        (*caten 3)
        (*pack-with
            (lambda (ignore1 op ignore2) op))

        (*parser <InfixFuncall>)
        (*parser <InfixArrayGet>)
        (*parser <InfixParen>)
        (*disj 3)
        
        (*caten 2)
        (*pack-with
            (lambda (minus val) (- val)))

        (*parser <InfixFuncall>)
        (*parser <InfixArrayGet>)
        (*parser <InfixParen>)
        (*disj 3)
        (*disj 2)
    done))

(define <InfixPow>
    (new
        ; Basic case - 2^3
        (*parser <InfixNeg>)
        (*parser <InfixPowerSymbol>)
        (*parser <InfixNeg>)
        (*caten 3)
        (*pack-with
            (lambda (a exp b) (list exp a b)))

        ; Handle multiple operations
        (*parser <InfixPowerSymbol>)
        (*parser <InfixNeg>)
        (*caten 2)*star

        (*caten 2)
        (*pack-with
            (lambda (base-operation other-operations) 
                (handle-multi-powers base-operation other-operations)))

        (*parser <InfixNeg>)
        (*disj 2)
    done))

(define <InfixMulDiv>
    (new
        ; Basic case - 2 * 3
        (*parser <InfixPow>)
        (*parser (find-next-operator #\* '* #\/ '/ ))
        (*parser <InfixPow>)
        (*caten 3)
        (*pack-with
            (lambda (exp1 op exp2) (list op exp1 exp2)))

        ; Handle multiple operations
        (*parser (find-next-operator #\* '* #\/ '/ ))
        (*parser <InfixPow>)
        (*caten 2) *star

        (*caten 2)
        (*pack-with
            (lambda (base-operation other-operations) 
                (handle-multi-operators base-operation other-operations)))

        (*parser <InfixPow>)
        (*pack (lambda (res) res))
        (*disj 2)
    done))

(define <InfixAddSub>
    (new

        ; Basic case - 2 + 3
        (*parser <InfixMulDiv>)
        (*parser (find-next-operator #\+ '+ #\- '-))
        (*parser <InfixMulDiv>)
        (*caten 3)
        (*pack-with
            (lambda (exp1 operator rest) (list operator exp1 rest)))

        ; Handle mutiple operations
        (*parser (find-next-operator #\+ '+ #\- '-))
        (*parser <InfixMulDiv>)
        (*caten 2) *star

        (*caten 2)
        (*pack-with
            (lambda (base-operation other-operations) 
                (handle-multi-operators base-operation other-operations)))

        (*parser <InfixMulDiv>)
        (*disj 2)

    done))

(define <InfixExpression>
    (new
        (*parser <InfixIgnore>)*star
        (*parser <InfixAddSub>)
        (*parser <InfixIgnore>)*star
        (*caten 3)
        (*pack-with
            (lambda (space1 exp space2) exp))
    done))

(define <InfixExtension>
    (new
        (*parser <InfixIgnore>)*star
        (*parser <InfixPrefixExtensionPrefix>)
        (*parser <InfixExpression>)
        (*caten 3)
        (*pack-with
            (lambda (space1 prefix expression) expression))
    done))


;;;;;;;;;;;;;; Complete Parser ;;;;;;;;;;;;;;

(define <sexpr>
  (new 
    (*parser <SexprIgnore>)*star

	(*parser <Boolean>)
	(*parser <Char>)
    (*parser <Number>)
    (*parser <String>)
    (*parser <Symbol>)
    (*parser <ProperList>)
    (*parser <ImproperList>)
    (*parser <Vector>)
    (*parser <Quoted>)
    (*parser <QuasiQuoted>)
    (*parser <UnQuoted>)
    (*parser <UnQuotedAndSpliced>)
	(*parser <CBName>)
    (*parser <InfixExtension>)
    (*disj 14)

	(*parser <SexprIgnore>)*star
	(*caten 3)
    (*pack-with
        (lambda (ignore1 input ignore2) input))
    done))
