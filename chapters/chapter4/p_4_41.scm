;(define (multiple-dwelling)
;  (let ((baker (amb 1 2 3 4 5))
;        (cooper (amb 1 2 3 4 5))
;        (fletcher (amb 1 2 3 4 5))
;        (miller (amb 1 2 3 4 5))
;        (smith (amb 1 2 3 4 5)))
;    (require
;      (distinct? (list baker cooper fletcher miller smith)))
;    (require (not (= baker 5)))
;    (require (not (= cooper 1)))
;    (require (not (= fletcher 5)))
;    (require (not (= fletcher 1)))
;    (require (> miller cooper))
;    (require (not (= (abs (- smith fletcher )) 1)))
;    (require (not (= (abs (- fletcher cooper )) 1)))
;    (list (list 'baker baker)
;          (list 'cooper cooper)
;          (list 'fletcher fletcher)
;          (list 'miller miller)
;          (list 'smith smith))))

; Normal Pragramming
; 乱数が上手く作れなかったので乱数を使わない方針でペアを求める
(use srfi-27)
(define r (random-source-make-integers (make-random-source)))

(define (distinct? ls)
     (cond
       ((null? ls) #t)
       ((memq (car ls) (cdr ls)) #f)
       (else (distinct? (cdr ls)))))

(define (dw? residents)
    (let ((baker (car residents))
          (cooper (cadr residents))
          (fletcher (caddr residents))
          (miller (cadddr residents))
          (smith (cadddr (cdr residents))))
      (if (and
             (distinct? (list baker cooper fletcher miller smith))
             (not (= baker 5))
             (not (= cooper 1))
             (not (= fletcher 5))
             (not (= fletcher 1))
             (> miller cooper)
             (not (= (abs (- smith fletcher )) 1))
             (not (= (abs (- fletcher cooper )) 1)))
           (list (list 'baker baker)
                 (list 'cooper cooper)
                 (list 'fletcher fletcher)
                 (list 'miller miller)
                 (list 'smith smith))
           #f)))

(define residents1 '(3 2 4 5 1))
(define res1 (dw? residents1))

(define residents2 '(3 2 5 4 1))
(define res2 (dw? residents2))

;gosh> res1
;((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;gosh> res2
;#f
