(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

;gosh> (cons (list 1 2) (cons (list 2 3) (list 4)))
;((1 2) (2 3) 4)
;gosh> (count-pairs (cons (list 1 2) (cons (list 2 3) (list 4))))
;7

; 同一のリストを区別できない
(define c1 (list 1 2))
(define copy-pairs (cons c1 c1))
; (count-pairs copy-pairs)
; 5
; 正しくは３

;循環するリストには対応することができない
(define circle-pair (list 1 2 3))
(set-cdr! (last-pair circle-pair) circle-pair)
; (count-pairs p1) ; 処理が終わらない
; 正しくは3が答え

;gosh> (car circle-pair)
;1
;gosh> (cdr circle-pair)
;#0=(2 3 1 . #0#)
;gosh> (eq? circle-pair circle-pair)
;#t
;gosh> (eq? (car circle-pair) (cdr circle-pair))
;#f
;gosh> (eq? (car circle-pair) (car circle-pair))
;#t

;---------------------------------------
; 3.17
;---------------------------------------
; memqで代用可能であった。。。。。
(define (include? target pairs)
  (display "called!")
  (cond
    ((null? pairs) #f)
    ((if (eq? target (car pairs))
       #t
       (include? target (cdr pairs))))))


(define (make-count-pairs walks)
  (define (count-pairs x)
    (cond ((not (pair? x)) 0)
          ((memq x walks) 0)
          (else
            (set! walks (cons x walks))
            (+ (count-pairs (car x))
               (count-pairs (cdr x))
               1))))
  count-pairs)

(define cp (make-count-pairs '()))

;gosh> (cp copy-pairs)
;1
;gosh> copy-pairs
;((1 2) 1 2)
;gosh> (cp circle-pair)
;3
;gosh> (eq? copy-pairs copy-pairs)
;#t
;gosh> (eq? copy-pairs (car copy-pairs))
;#f
