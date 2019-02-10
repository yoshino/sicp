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

;---------------------------------------------------------
; a: valueが'*unassained*であればエラーを発生させる
;---------------------------------------------------------
(define (lookup-variable-value var env)
   (define (env-loop env)
      (define (scan vars vals)
               (cond ((null? vars)
                      (env-loop (enclosing-environment env)))
                     ((eq? var (car vars))
                      (if (eq? (car vals) '*unassaigned*)
                          (error "Anassigned value:" (car vars))
                          (car vals)))
                     (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error " Unbound variable " var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
   (env-loop env))

(define vars '(a b c d))
(define vals '(1 2 3 4))
(define f1 (make-frame vars vals))
(define vars '(e f g h))
(define vals '(*unassaigned* 6 7 8))
(define f2 (make-frame vars vals))
(define e (list f1 f2))
;(((a b c d) 1 2 3 4) ((e f g h) *unassaigned* 6 7 8))
;(define lookup-res (lookup-variable-value 'e e))

;---------------------------------------------------------
; b: 内部手続きを持たない等価な手続きを返す
;---------------------------------------------------------

;(define (f x)
;  (define (even? n) (if (= n 0) true (odd? (- n 1))))
;  (define (odd? n) (if (= n 0) false ( even? (- n 1))))
;  'rest-body)

; こんな感じに変換したい

;(let (((even? n) '*unassaigned*)
;      ((odd? n) '*unassaigned*))
;  (set! (even? n) (if (= n 0) true (odd? (- n 1))))
;  (set! (odd? n) (if (= n 0) false ( even? (- n 1)))))

; サンプル
(define body
  '((define (even? n) (if (= n 0) true (odd? (- n 1))))
    (define (odd? n) (if (= n 0) false (even? (- n 1))))
    'rest-body))

(define (scan-out-defines body)
  (define (iter b inner-defines)
    (cond
      ((not (pair? b)) inner-defines)
      ((if (eq? 'define (car (car b)))
           (iter (cdr b) (cons (list (cadr (car b)) (caddr (car b))) inner-defines))
           (iter (cdr b) inner-defines)))))

  (define (unassaigned-list vars res)
    (if (null? vars)
        res
        (unassaigned-list (cdr vars) (cons (list (car vars) '*unassaigned*) res))))

  (define (set-berfore-lambda vars vals)
    (define (iter vars vals res)
      (if (null? vars)
          res
          (iter (cdr vars) (cdr vals) (cons (list 'set! (car vars) (car vals)) res))))
    (iter vars vals '()))

  (let ((inner-defines (iter body '())))
    (cons 'let
          (list (unassaigned-list (map car inner-defines) '())
                (set-berfore-lambda (map car inner-defines) (map cdr inner-defines))))))


(define result (scan-out-defines body))
;gosh> result
;(let (((even? n) *unassaigned*) ((odd? n) *unassaigned*)) ((set! (even? n) ((if (= n 0) true (odd? (- n 1))))) (set! (odd? n) ((if (= n 0) false (even? (- n 1)))))))

;----------------------------------------------------------------------------------
; c. scan-out-defines は procedure-bodyとmake-procedureのどちらに組み込むべき？
;----------------------------------------------------------------------------------
(define (make-procedure parameters body env)
 (list 'procedure parameters body env))

; procedure-bodyに組み込むべき
(define (procedure-body p) (caddr p))
; モジュール性を高めるので
(define (procedure-body p)
  (scan-out-defines (caddr p)))

; 効率の意味では、

;procedure-body に組み込むと apply と user-print 手続きで呼ばれるのに対し、
;make-procedure に組み込めば eval の際のみに呼ばれるから make-procedure がよい

; らしい

