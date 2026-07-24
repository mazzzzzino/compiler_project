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
  (emit "    movl $~s, %eax"(immediate-rep x))

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


