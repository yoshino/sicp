; 4.6で定義したlet
(define (cons-car pairs)
  (let ((result '()))
    (define (loop pairs)
      (if (null? pairs)
        result
        (begin
          (set! result (append result (list (car (car pairs)))))
          (loop (cdr pairs)))))
    (loop pairs)))

(define (cons-cdr pairs)
  (let ((result '()))
    (define (loop pairs)
      (if (null? pairs)
        result
        (begin
          (set! result (append result (cdr (car pairs))))
          (loop (cdr pairs)))))
    (loop pairs)))

(define (let->combination exp)
  (define hash (apply list (car (cdr exp))))
  (define proc (apply list (car (cdr (cdr exp)))))
  (define vals (cons-cdr hash))
  (define keys (cons-car hash))
  (apply (eval (append (cons 'lambda (list keys)) (list proc)) (interaction-environment)) vals))

; let*
; それまでの束縛が全てみえる
(define res-let* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)))
;39

;------------------------------------------------------------
;let* 式を入れ子の let 式の集合として書き直す方法
;------------------------------------------------------------
(define e '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)))

(define (last-exp? exp) (null? (cdr exp)))
(define (let-body exp) (caddr exp))
(define (let*->nested-lets exp)
  (define (make-let params)
    (cond ((last-exp? params) (append (list 'let (list (car params))) (list (let-body exp))))
          (else (list 'let (list (car params)) (make-let (cdr params))))))
    (make-let (cadr exp)))

(define will-evaluate (let*->nested-lets e))
(define res (eval will-evaluate (interaction-environment)))
; 39

