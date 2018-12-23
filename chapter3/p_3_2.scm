(define (make-monitored func)
  (define counter 0)
  (lambda (x)
    (cond
      ((eq? x 'how-many-calls?) counter)
      ((eq? x 'reset-count)
       (begin (set! counter 0)
              counter))
      (else (begin (set! counter (+ counter 1))
                   (func x))))))

(define s (make-monitored sqrt))

;gosh> (s 100)
;10
;gosh> (s 'how-many-calls?)
;1
;gosh> (s 'reset-count)
;0
;gosh> (s 'how-many-calls?)
;0
