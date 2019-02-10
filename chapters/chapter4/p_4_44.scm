; “8 クイーンパズル” とは、どのクイーンもほかのクイーンの利き
; 筋に入らない (つまり、同じ行・列・対角線の上に二つのクイーン
; があるということがないようにする) ように 8 個のクイーンをチ
; ェス盤の上に置く方法を問うものである。

(define (make-queen row col) (list row col))

(define (8queen? queens)
  ; selector
  (define (row queen) (car queen))
  (define (col queen) (cdr queen))

  ; filter
  (define (distinct? ls) (and (distinct-row? ls) (distinct-col? ls)))
  (define (distinct-row? ls)
    (cond
      ((null? ls) #t)
      ((memq (row (car ls)) (map row (cdr ls))) #f)
      (else (distinct-row? (cdr ls)))))
  (define (distinct-col? ls)
    (cond
      ((null? ls) #t)
      ((memq (car (col (car ls))) (map car (map col (cdr ls)))) #f)
      (else (distinct-col? (cdr ls)))))

  (let ((q1 (car queens))
        (q2 (cadr queens))
        (q3 (caddr queens))
        (q4 (cadddr queens))
        (q5 (cadddr (cdr queens)))
        (q6 (cadddr (cdr (cdr queens))))
        (q7 (cadddr (cdr (cdr (cdr queens)))))
        (q8 (cadddr (cdr (cdr (cdr (cdr queens)))))))
    (if (distinct? (list q1 q2 q3 q4 q5 q6 q7 q8))
        (list q1 q2 q3 q4 q5 q6 q7 q8)
        #f)))

; normal
(define true-qs (list (make-queen 1 1) (make-queen 2 2) (make-queen 3 3) (make-queen 4 4)
                      (make-queen 5 5) (make-queen 6 6) (make-queen 7 7) (make-queen 8 8)))
(define false-qs (list (make-queen 1 1) (make-queen 2 2) (make-queen 3 3) (make-queen 4 4)
                       (make-queen 5 5) (make-queen 6 6) (make-queen 7 6) (make-queen 8 8)))

;gosh> (8queen? true-qs)
;((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8))
;gosh> (8queen? false-qs)
;#f

; ちなみに上記の方法は非効率的な手法。
; 効率性を求めるとするとq1,q2の段階でdisistinct?を走らせたり段階的に求めていくほうが条件分岐が抑えられる
