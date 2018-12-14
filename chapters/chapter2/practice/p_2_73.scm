(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    (else ((get 'deriv (operator exp))
           (operans exp) var))))

(define (operator exp) (car exp))
(define (operans exp) (cdr exp))

; 和
(define (install-sum-package)
  (define (append s) (car s))
  (define (augend s)
    (if (null? (cddr s))
      (cadr s)
      (cons '+ (cdr s))))
  (define (make-sum a1 a2)
    (cond
      ((=number? a1 0) a2)
      ((=number? a2 0) a1)
      ((and (number? a1) (number? a2)) (+ a1 a2))
      (else (list '+ a1 a2))))
  (define (deriv-sum exp var)
    (make-sum (deriv (append exp) var)
              (deriv (append exp) var)))

  ; interface
  (put 'make '+ make-sum)
  (put 'deriv '+ deriv-sum)
  'done)

; get guest --> interface --> server
;     guest <-- interface <-- server put
