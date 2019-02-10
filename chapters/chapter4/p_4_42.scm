(use srfi-27)

(define (distinct? ls)
     (cond
       ((null? ls) #t)
       ((memq (car ls) (cdr ls)) #f)
       (else (distinct? (cdr ls)))))

; 排他的論理和
(define (xor cond1 cond2)
  (if (or (and cond1 (not cond2))
          (and (not cond1) cond2))
      #t
      #f))

(define (rank? students)
    (let ((betty (car students))
          (ethel (cadr students))
          (joan (caddr students))
          (kitty (cadddr students))
          (mary (cadddr (cdr students))))
      (if (and
             (distinct? (list betty ethel joan kitty mary))
             (xor (= kitty 2) (= betty 3))
             (xor (= ethel 1) (= joan 2))
             (xor (= joan 3) (= ethel 5))
             (xor (= kitty 2) (= mary 4))
             (xor (= mary 4) (= betty 1)))
           (list (list 'betty betty)
                 (list 'ethel ethel)
                 (list 'joan joan)
                 (list 'kitty kitty)
                 (list 'mary mary))
           #f)))

(define students1 '(3 2 4 5 1))
(define res1 (rank? students1))

(define students2 '(3 2 5 4 1))
(define res2 (rank? students2))

(define students3 '(3 5 2 1 4))
(define res3 (rank? students3))

;gosh> res1
;#f
;gosh> res2
;#f
;gosh> res3
;((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))
