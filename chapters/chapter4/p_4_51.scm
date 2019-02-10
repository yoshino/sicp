(define (require p) (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define count 0)

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

; 通常のset!を使うと値が更新されない

;;; Starting a new problem
;;; Amb-Eval value:
(a c 1)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(a b 1)


;以下のように実装
(define (analyze-parmanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                       (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            ;(set-variable-value! var old-value env)
                            (fail2)))))
               fail))))


(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (parmanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

;;; Starting a new problem
;;; Amb-Eval value:
(a b 1)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(a c 2)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(b a 4)

