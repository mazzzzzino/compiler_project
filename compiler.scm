;emit deos simple printf thing and sends the output stream
;#t tells about the current port
(define emit (lambda args ( apply simple-format #t args)
		     (newline)))

(define (compile-program x)
  (unless (integer? x) (error "not an integer"))
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
  (emit "    movl $~s, %eax" x)

  ;restore state for return to C

  (emit "    pop  %esi")
  (emit "    pop  %edi")
  (emit "    pop  %edx")
  (emit "    ret"))


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

