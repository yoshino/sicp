; nest構造がないリスト
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (no-nest-list? items)
  (and (list? items) (= (count-leaves items) (length items))))

; (appned (list 1 2 3) (list 4 5 6))
; (1 2 3 4 5 6)
(define l (list (list 1 2 3) (list 4 5 6)))
(define (figure items)
  (append (car items) (car (cdr items))))

(define l2 (list l l))
; (((1 2 3) (4 5 6)) ((1 2 3) (4 5 6)))

(define (figure items)
  (cond
    ((no-nest-list? items) items)
    (else (append (figure (car items)) (figure (cdr items))))))

; gosh> l2
; (((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))
; gosh> (figure l2)
; (1 2 3 4 5 6 1 2 3 4 5 6)
