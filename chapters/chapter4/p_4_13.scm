(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env ))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define vars '(a b c d))
(define vals '(1 2 3 4))
(define f1 (make-frame vars vals))

(define vars '(e f g h))
(define vals '(5 6 7 8))
(define f2 (make-frame vars vals))

(define e (list f1 f2))
;(((a b c d) 1 2 3 4) ((e f g h) 1 2 3 4))

(define (lookup-variable-value var env)
   (define (env-loop env)
            (define (scan vars vals)
                     (cond ((null? vars)
                            (env-loop (enclosing-environment env)))
                           ((eq? var (car vars)) (car vals))
                           (else (scan (cdr vars) (cdr vals)))))
            (if (eq? env the-empty-environment)
                (error " Unbound variable " var)
                (let ((frame (first-frame env)))
                  (scan (frame-variables frame)
                        (frame-values frame)))))
   (env-loop env))
(define lookup-res (lookup-variable-value 'f e))
; 6

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals )))))
    (if (eq? env the-empty-environment)
        (error " Unbound variable : SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(set-variable-value! 'h 12 e)
;e
;=>(((a b c d) 1 2 3 4) ((e f g h) 5 6 7 12))

(define (define-variable! var val env)
         (let ((frame (first-frame env)))
           (define (scan vars vals)
                    (cond ((null? vars)
                           (add-binding-to-frame! var val frame ))
                          ((eq? var (car vars)) (set-car! vals val))
                          (else (scan (cdr vars) (cdr vals)))))
           (scan (frame-variables frame) (frame-values frame))))
(define-variable! 'k 100 e)
;gosh> e
;(((k a b c d) 100 1 2 3 4) ((e f g h) 5 6 7 12))

;-------------------------------------------------------------
; 4.13 変数の束縛を解放する
;-------------------------------------------------------------
(define (make-unbound! var env)
   (define (env-loop env)
            (define (scan vars vals)
                     (cond ((null? vars)
                            (env-loop (enclosing-environment env)))
                            ((eq? var (car vars)) (begin
                                                    (set-car! vals (car (cdr vals)))
                                                    (set-cdr! vals (cddr vals))
                                                    (set-car! vars (car (cdr vars)))
                                                    (set-cdr! vars (cddr vars))))
                           (else (scan (cdr vars) (cdr vals)))))
            (if (eq? env the-empty-environment)
                (error " Unbound variable " var)
                (let ((frame (first-frame env)))
                  (scan (frame-variables frame)
                        (frame-values frame)))))
   (env-loop env))
;e
;(((k q a b c d) 100 12 1 2 3 4) ((e f g h) 5 6 7 12))
;(make-unbound! 'f e)
;gosh> e
;(((k q a b c d) 100 12 1 2 3 4) ((e g h) 5 7 12))

; このままだと複数のフレームに定義されている時に対応できない
(define (make-unbound! var env)
   (define (env-loop env)
            (define (scan vars vals)
                     (cond ((null? vars)
                            (env-loop (enclosing-environment env)))
                            ((eq? var (car vars)) (begin
                                                    (set-car! vals (car (cdr vals)))
                                                    (set-cdr! vals (cddr vals))
                                                    (set-car! vars (car (cdr vars)))
                                                    (set-cdr! vars (cddr vars))
                                                    (make-unbound! var env)))
                           (else (scan (cdr vars) (cdr vals)))))
            (if (eq? env the-empty-environment)
                'ok
                (let ((frame (first-frame env)))
                  (scan (frame-variables frame)
                        (frame-values frame)))))
   (env-loop env))

(define vars '(a b f d))
(define vals '(1 2 3 4))
(define f1 (make-frame vars vals))

(define vars '(e f g h))
(define vals '(5 6 7 8))
(define f2 (make-frame vars vals))

(define e (list f1 f2))
(make-unbound! 'f e)
;gosh> e
;(((a b d) 1 2 4) ((e g h) 5 7 8))

;この実装は必要ない
;正解は最初のフレームだけを開放すれば良い。
;他のフレームは何かの関数のために用意された束縛された変数かもしれないので。
