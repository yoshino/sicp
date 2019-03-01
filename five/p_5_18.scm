(load "./machine_trace-register")

;; 今回の実装は、make-machineする時にtraceするレジスタを指定するようにした。


;; 以下の様に手続きを持たせてメッセージパッシングするのが一番ラクだった.
(define (make-register name)
  (let ((contents '*unassigned*)
        (register-trace-flag #f))
       (define (set-register-trace flag)
         (set! register-trace-flag flag))
       (define (dispatch message)
         (cond ((eq? message 'get)
                (if register-trace-flag
                    (print "get register [" name "]: " contents))
                contents)
               ((eq? message 'set)
                (lambda (value)
                        (if register-trace-flag
                            (print "set register [" name "]: " contents " to " value))
                        (set! contents value)))
               ((eq? message 'trace-on) (set-register-trace #t))
               ((eq? message 'trace-off) (set-register-trace #f))
               (else
                 (error "Unknown request -- REGISTER" message))))
       dispatch))


;;再帰階乗マシン: デバッグ用
(define fact-machine
  (make-machine
     (list (list 'n #f) (list 'continue #f) (list 'val #t)) ;; register with flag
     (list (list '= =) (list '- -) (list '* *))   ;; operations
     '(controller
       (assign continue (label fact-done))
        ;set up final return address
        fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        ;; Set up for the recursive call by saving n and continue.
        ;; Set up continue so that the computation will continue
        ;; at after-fact when the subroutine returns.
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
        after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
        (goto (reg continue))
        ;return to caller
        base-case
        (assign val (const 1))
        ;base case: 1! = 1
        (goto (reg continue))
        ;return to caller
        fact-done)))

(set-register-contents! fact-machine 'n 6)
(start fact-machine)
(get-register-contents fact-machine 'val)
