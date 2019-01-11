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

;--------------------------------------------------------------------
; 名前付きlet
;--------------------------------------------------------------------
; 引数がつまり
; (let name ((let-args,,,　みたいになる時

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

; (let name (let-define.... (let-body)
(define e
  '(let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

;gosh> (cadr e)
;fib-iter
;gosh> (caddr e)
;((a 1) (b 0) (count n))
;gosh> (cadddr e)
;(if (= count 0) b (fib-iter (+ a b) a (- count 1)))

; 名前付きletはdefineを定義に入れるのがポイント
(define (let->combination exp)
  (define name (cadr exp))
  (define hash (apply list (caddr exp)))
  (define vals (cons-cdr hash))
  (define keys (cons-car hash))
  (define let-body (cadddr exp))
  (list (list 'define (cons name (list keys)) (append (cons 'lambda (list keys)) (list let-body)))))

(define res (let->combination e))
;gosh> res
;((define (fib-iter (a b count)) (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))))
