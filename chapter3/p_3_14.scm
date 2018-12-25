(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((tmp (cdr x)))
        (set-cdr! x y)
        (loop tmp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

;gosh> w
;(d c b a)
;gosh> v
;(a)

;(a b c d)
;gosh> (set-cdr! v '())
;gosh> v
;(a)

; loopをたどると
;(loop (a b c d) ())
;(loop (b c d  ) (a))
;(loop (c d    ) (b a))
;(loop (d      ) (c a b))
;(loop (       ) (d c b a))

;ポインタは
;v -> a
;w -> d-c-b-a

;vに関しては、(loop (list 'a 'b 'c 'd) '())の結果が反映されている
;wのaを指している。
