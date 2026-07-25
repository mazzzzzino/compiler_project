;emit deos simple printf thing and sends the output stream
;#t tells about the current port
(define emit (lambda args ( apply simple-format #t args)
		     (newline)))

(define (compile-program x)
 
  (emit "    .text")
  (emit "    .p2align 4,,15")
  (emit "    .globl scheme_entry")
  (emit "    .type scheme_entry, @function")
  (emit "scheme_entry:")

  ;handle incoming call from C

  (emit "    push %esi")
  (emit "    push %edi")
  (emit "    push %edx")
  
  ;our code goes here
  (compile-expr x )

  ;restore state for return to C

  (emit "    pop  %esi")
  (emit "    pop  %edi")
  (emit "    pop  %edx")
  (emit "    ret"))


;what we did till now was just for fixnums integers but scheme has more to it than just integers 
;like booleans, characters, null values things like pairs, vec,lists and strings
;storing everything om the heap introduces unnecessary complexity
;so here we use the trick "tagged pointers" which sacrifices some bits to store val which 
;helps us in identifying what it really is 

;integer
(define fixnum-mask 3)
(define fixnum-shift 2)

;boolean
(define bool-mask 255)
(define bool-shift 8)
(define bool-tag 7)

;char
(define char-mask 255)
(define char-shift 8)
(define char-tag 15)

;pointer
(define ptr-mask 7)
(define ptr-mask-val #xfffffff8 )

;can tell about the type pointer refers to
(define pair-tag 1)
(define vec-tag 2)
(define str-tag 3)
(define cls-tag 5)
(define syb-tag 6)


;utility function which does the operation and converts them into representation
(define (immediate-rep x)
  (cond ((integer? x)(logand (ash x fixnum-shift) #xffffffff))
	((char? x)(logior (ash (char->integer x) char-shift) char-tag))
	((boolean? x)
	 (if x
	     (logior ( ash 1 bool-shift ) bool-tag)
	     bool-tag))
	))


;checks whether it is primitive call or not -> car(primcall + 1 2)returns primcall
;then using eq? checks if it is or not
(define (primitive-call? form)
  (eq? 'primcall (car form)))

;cadr is just car(cdr()) -> car(cdr(primcall + 1 2)) -> car(+ 1 2)-> +  
(define (primitive-op form) (cadr form ))

;caddr -> car(cdr(cdr())) gives us the first argument
(define (primitive-op-arg1 form) (caddr form))

;cadddr -> car(cdr(cdr(cdr()))) gives us second argumeent
(define (primitive-op-arg2 form) (cadddr form))

;cddr -> cdr(cdr()) -> cdr(cdr(primcall + 1 2)) -> cdr(+ 1 2) -> (1 2)
(define (primitive-op-args form) (cddr form))


(define (immediate? x) (or (integer? x) (char? x) (boolean? x) (null? x)))


(define (compile-expr e)
  (cond
  ((immediate? e) (emit "    movl $~s, %eax" (immediate-rep e)))
  ((primitive-call? e) (compile-primitive-call e))))

(define (compile-primitive-call form)
  (case (primitive-op form)
    ((add1)
     ;2 operations below first one stores the arg in the register
     ;then second one just makes the assembly -> addl $~1, %eax 
     (compile-expr (primitive-op-arg1 form))
     (emit "    addl $~s, %eax" (immediate-rep 1)))
    ;similar to add1
    ((sub1)
    (compile-expr (primitive-op-arg1 form))
    (emit "    subl $~s, %eax" (immediate-rep 1)))
  ))



;here calls the compiler-program and redirects the ouput to the /tmp/compiled.s file
;then runs the gcc command 
;gcc creates a object out of compiled.s  and main_runtime.c then links them together into an executable 
(define (compile-to-binary program)
  (begin
    (with-output-to-file "/tmp/compiled.s"
			 (lambda () (compile-program program)))
    (system "gcc -fomit-frame-pointer -m32 /tmp/compiled.s main_runtime.c")))

;prints the output on the screen
(define (compile-run program)
 ( begin (compile-to-binary program)
  (system "./a.out")))


