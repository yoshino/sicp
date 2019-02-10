(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env ))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define ( add-binding-to-frame! var val frame)
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

; 以下の３つの処理を抽象化する
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

; 教科書に乗ってたこの定義は外側のフレームを探しに行っていないので
; 不完全にみえる
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


; 共通パターンを抽象化して取り出す
(define (scan var vars vals)
  (cond ((null? vars) '())
        ((eq? var (car vars)) vals)
        (else
          (scan var (cdr vars) (cdr vals)))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
             (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
                  (if (null? result-of-scan)
                      (env-loop (enclosing-environment env))
                      (car result-of-scan))))))
  (env-loop env))
(define lookup-res (lookup-variable-value 'f e))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
             (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
                  (if (null? result-of-scan)
                      (env-loop (enclosing-environment env))
                      (set-car! result-of-scan val))))))
  (env-loop env))
(set-variable-value! 'h 22 e)
