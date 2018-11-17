; branch
(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

; mobile
(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-weight branch)
   (if (pair? (branch-structure branch))
       (total-weight (branch-structure branch))
       (branch-structure branch)))

(define (total-weight mobile)
   (+ (branch-weight (left-branch mobile))
      (branch-weight (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (branch-balanced? branch)
  (if (pair? (branch-structure branch))
    (balanced? (branch-structure branch))
    #t))

(define (balanced? mobile)
  (and  (= (branch-torque (left-branch mobile))
           (branch-torque (right-branch mobile)))
        (branch-balanced? (left-branch mobile))
        (branch-balanced? (right-branch mobile))))

; example
(define m1 (make-mobile (make-branch 2 3) (make-branch 2 3)))
(define m2 (make-mobile (make-branch 2 3) (make-branch 4 5)))
(define m3 (make-mobile (make-branch 5 m1) (make-branch 3 m2)))

;gosh> m1
;((2 3) (2 3))
;gosh> m2
;((2 3) (4 5))
;gosh> m3
;((5 ((2 3) (2 3))) (3 ((2 3) (4 5))))

;gosh> (balanced? m1)
;#t
;gosh> (balanced? m2)
;#f
;gosh> (balanced? m3)
;#f

; 以下のようにconsを使って書き直した時、何を修正すれば良いか？
; branch
(define (make-branch length structure)
  (cons length structure))

(define (branch-structure branch)
  (cdr branch))

; mobile
(define (make-mobile left right)
  (cons left right))

(define (right-branch mobile)
  (cdr mobile))

(define m1 (make-mobile (make-branch 2 3) (make-branch 2 3)))
(define m2 (make-mobile (make-branch 2 3) (make-branch 4 5)))
(define m3 (make-mobile (make-branch 5 m1) (make-branch 3 m2)))
