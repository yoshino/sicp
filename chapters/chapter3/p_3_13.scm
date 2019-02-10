(define (last-pair? x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;gosh> (last-pair (list 'a 'b 'c))
;(c)

;gosh> (define l (list 'a 'b 'c))
;gosh> (set-cdr! l (list 'd 'e 'f))
;gosh> l
;(a d e f)

;gosh> (define l2 (list 'a 'b 'c))
;gosh> (set-car! l2 (list 'd 'e 'f))
;gosh> l2
;((d e f) b c)


(define z (make-cycle (list 'a 'b 'c)))

; 以下のような結果になった
; gosh> z
; #0=(a b c . #0#)

;x ->  a -> b -> c -> a.....先頭へポインタが指すのでループする
