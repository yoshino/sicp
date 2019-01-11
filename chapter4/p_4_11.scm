;フレームをリストのペアとして表現するのではなく、束縛のリストとして表現することもできる。
;この場合、それぞれの束縛は名前と値のペアとなる。
;環境演算を書き直し、この表現を使うようにせよ

;フレームをリストのペアとして表現する
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define vars '(a b c d))
(define vals '(1 2 3 4))
(define f (make-frame vars vals))

;gosh> f
;((a b c d) 1 2 3 4)
;gosh> (frame-variables f)
;(a b c d)
;gosh> (frame-values f)
;(1 2 3 4)

(define (make-frame vars vals)
  (define (set-vars vars vals res)
    (if (and (null? vars) (null? vals))
        res
        (set-vars (cdr vars)
                  (cdr vals)
                  (cons (list (car vars) (car vals)) res))))
  (set-vars vars vals '()))

(define vars '(a b c d))
(define vals '(1 2 3 4))
(define f (make-frame vars vals))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map car (map cdr frame)))

;gosh> (frame-variables f)
;(d c b a)
;gosh> (frame-values f)
;(4 3 2 1)

; mapをうまく利用した別解
(define (make-frame vars vals) (map cons vars vals))
;gosh> (make-frame vars vals)
;((a . 1) (b . 2) (c . 3) (d . 4))
;gosh> (define f (make-frame vars vals))
;f
;gosh> (map car f)
;(a b c d)
;gosh> (map cdr f)
;(1 2 3 4)
