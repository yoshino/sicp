(load "./eval_amb.scm")

; print
(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

;(define (driver-loop)
; (prompt-for-input input-prompt)
; (let ((input (read)))
;  (let ((output (eval input the-global-environment)))
;;   (announce-output output-prompt)
;   (user-print output)))
; (driver-loop))
(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      ;try-again 手続きを引数として内部ループを再起動
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline) (display ";;; Starting a new problem ")
            (ambeval
              input
              the-global-environment
              ;; ambeval success
              (lambda (val next-alternative)
                (announce-output output-prompt)
                (user-print val)
                (internal-loop next-alternative))
              ;; ambeval failure
              (lambda ()
                (announce-output
                  ";;; There are no more values of")
                (user-print input)
                (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline) (display ";;; There is no current problem")
      (driver-loop))))

(define (prompt-for-input string)
 (newline) (newline) (display string) (newline))

(define (announce-output string)
 (newline) (display string) (newline))

(define (user-print object)
 (if (compound-procedure? object)
  (display (list 'compound-procedure
                 (procedure-parameters object)
                 (procedure-body object)
                 '<procedure-env>))
  (display object)))

(define the-global-environment (setup-environment))

(driver-loop)
