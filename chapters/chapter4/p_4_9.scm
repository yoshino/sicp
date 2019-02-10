; while: while (condition) (iの初期値) (iの間隔) (実行文)
(define e '(while (< i 5) i 0 (+ i 1) (display i)))

;gosh> (cadr e)
;(> i 5)
;gosh> (caddr e)
;i
;gosh> (cadddr e)
;0
;gosh> (cadddr (cdr e))
;(+ i 1)
;gosh> (cadddr (cdr (cdr e)))
;(display i)

(define e '(while (< i 5) i 0 (+ i 1) (display i)))
(define (condition e) (cadr e))
(define (iter e) (caddr e))
(define (start-value e) (cadddr e))
(define (step e) (cadddr (cdr e)))
(define (clausure e) (cadddr (cdr (cdr e))))

(define (loop exp)
  (cons 'define
        (list (list 'loop)
              (append (list 'if (condition exp))
                      (list (list 'begin
                                  (clausure exp)
                                  (list 'set! (iter exp) (step exp))
                                  'loop))))))

(define (while exp)
  (cons 'define
        (cons 'while
        (list (list 'while)
              (list 'define (iter exp) (start-value exp))
              (loop exp)))))

;gosh> (while e)
;(define loop (while) (define i 0) (define (loop) (if (< i 5) (begin (display i) (set! i (+ i 1)) loop))))
