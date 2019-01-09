;------------------------------------------------------------
; and
;------------------------------------------------------------
;interpreterの中でこんな感じに使いたい
;((if? exp) (eval-and exp env))

(define val1 10)
(define val2 20)
(define expression '(and (= val1 10) (= val2 20) (+ val1 val2)))

;gosh> (eval (cadr (car expression)) (interaction-environment))
;#t

(define (eval-and exp)
  (cond
    ((eq? (car exp) 'and) (eval-and (cdr exp)))
    ((null? (cdr exp)) (eval (car exp) (interaction-environment)))
    (else (if (eval (car exp) (interaction-environment))
              (eval-and (cdr exp))
              #f))))

(define expression '(and (= val1 10) (= val2 20) (+ val1 val2)))
(define and-res (eval-and expression))
;gosh> and-res
;30

(define expression '(and (= val1 10) (= val2 20) #f (+ val1 val2)))
(define and-res (eval-and expression))
;gosh> and-res
;#f

;------------------------------------------------------------
; or
;------------------------------------------------------------
;((or? exp) (eval-or exp env))

(define val1 20)
(define val2 20)
(define expression '(or (= val1 10) (= val2 10) (+ val1 val2) (- val1 val2)))

(define (eval-or exp)
  (let ((order (eval (car exp) (interaction-environment))))
    (cond
      ((eq? (car exp) 'or) (eval-or (cdr exp)))
      ((null? (cdr exp)) order)
      (else (if order
                order
                (eval-or (cdr exp)))))))

(define or-res (eval-or expression))
;gosh> or-res
;40
