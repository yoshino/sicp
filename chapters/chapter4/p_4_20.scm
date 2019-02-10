(define e '(letrec
             ((fact (lambda (n)
                      (if (= n 1) 1 (* n (fact (- n 1)))))))
                    (fact 10)))

;上記がこのように変換するようにしたい
(let
  ((fact '*unassigned*))
  (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))
  (fact 5))

(define res
  (let
    ((fact '*unassigned*))
    (begin
      (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))))
    (fact 5)))
;120

(define e2 '(letrec
              ((fact1 (lambda (n) (if (= n 1) 1 (* n (fact1 (- n 1))))))
               (fact2 (lambda (n) (if (= n 2) 2 (* n (fact2 (- n 2)))))))
                    (fact1 10)))

;上記がこのように変換するようにしたい
(let
  ((fact1 '*unassigned*)
   (fact2 '*unassigned*))
  (set! fact1 (lambda (n) (if (= n 1) 1 (* n (fact1 (- n 1))))))
  (set! fact2 (lambda (n) (if (= n 2) 2 (* n (fact1 (- n 2))))))
  (fact1 5))


(define func (cadr (cdr e2)))
(define vars (map car (car (cdr e2))))
(define vals (map car (map cdr (car (cdr e2)))))

(define (set-anassigned vars)
  (define (iter vars res)
    (if (null? vars)
        res
        (iter (cdr vars) (cons (list (car vars) ''*unassigned*) res))))
  (iter vars '()))
(define unassigned-vars (set-anassigned vars))
;((fact2 *unassigned*) (fact1 *unassigned*))

(define (set-vals vars vals)
  (define (iter vars vals res)
    (if (null? vars)
        res
        (iter (cdr vars) (cdr vals) (cons (list 'set! (car vars) (car vals)) res))))
  (iter vars vals '()))
(define setted-vals (cons 'begin (set-vals vars vals)))

(define result (list 'let unassigned-vars setted-vals func))
;(let ((fact2 '*unassigned*) (fact1 '*unassigned*)) (begin (set! fact2 (lambda (n) (if (= n 2) 2 (* n (fact2 (- n 2)))))) (set! fact1 (lambda (n) (if (= n 1) 1 (* n (fact1 (- n 1))))))) (fact1 10))
;gosh> (eval result (interaction-environment))
;3628800

; a: 答え
(define (analyze-letrec exp)
  (define (set-anassigned vars)
    (define (iter vars res)
      (if (null? vars)
          res
          (iter (cdr vars) (cons (list (car vars) ''*unassigned*) res))))
    (iter vars '()))

  (define (set-vals vars vals)
    (define (iter vars vals res)
      (if (null? vars)
          res
          (iter (cdr vars) (cdr vals) (cons (list 'set! (car vars) (car vals)) res))))
    (iter vars vals '()))

  (let
    ((func (cadr (cdr exp)))
     (vars (map car (car (cdr exp))))
     (vals (map car (map cdr (car (cdr exp))))))
    (let
      ((unassigned-vars (set-anassigned vars))
       (setted-vals (cons 'begin (set-vals vars vals))))
      (list 'let unassigned-vars setted-vals func))))

(define letrec-res (analyze-letrec e))
;gosh> (eval letrec-res (interaction-environment))
;3628800

; b
;(define (f x)
;  (letrec
;    ((even? (lambda (n)
;              (if (= n 0) true (odd? (- n 1)))))
;     (odd? ( lambda (n)
;                    (if (= n 0) false (even? (- n 1))))))
;    ⟨f の本体の残り⟩))

; これをdefineを使って定義すると、本体の残りもeven? odd?と同一環境にあるので、
; UndefinedErrorがでてしまう
;(define (f x)
;  (define (even? (lambda (n) (if (= n 0) true (odd? (- n 1))))))
;  (define (odd? ( lambda (n) (if (= n 0) false (even? (- n 1))))))
;  ⟨f の本体の残り⟩))

; letを使うと
; let評価時点で新しい環境が作成されそこでeven?が評価されるが、
; even?を評価する時にodd?が評価されていないのでUndefined Errorがでてしまう
;(define (f x)
;  (let ((even? (lambda (n) (if (= n 0) true (odd? (- n 1))))))
;       ((define (odd? ( lambda (n) (if (= n 0) false (even? (- n 1)))))))
;  ⟨f の本体の残り⟩))
