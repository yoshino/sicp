; originally from https://gist.github.com/mururu/a27918cb98cbbe213dca
(load "./eval.scm")

; print
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
 (prompt-for-input input-prompt)
 (let ((input (read)))
  (let ((output (time (eval input the-global-environment)))) ; timeを組み込む
   (announce-output output-prompt)
   (user-print output)))
 (driver-loop))

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

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1)))
    )
  (iter 1 0 n))

; (driver-loop) の中で以下のように実証する
;;; M-Eval input:
;(define (fib n)
;  (define (iter a b count)
;    (if (= count 0)
;      b
;      (iter (+ a b) a (- count 1)))
;    )
;  (iter 1 0 n))
;;(time (eval input the-global-environment))
;; real   0.000
;; user   0.000
;; sys    0.000
;
;;;; M-Eval value:
;ok
;
;;;; M-Eval input:
;(fib 1000)
;;(time (eval input the-global-environment))
;; real   0.008
;; user   0.010
;; sys    0.000
;
;;;; M-Eval value:
;43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
