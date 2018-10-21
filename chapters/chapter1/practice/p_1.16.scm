;----------------------------
; 反復プロセス
;----------------------------
;(define (expt b n)
;  (expt-iter b n 1))

;(define (expt-iter b counter product)
;  (if (= counter 0)
;    product
;    (expt-iter b
;               (- counter 1)
;               (* b product))))


;----------------------------
; 再帰プロセス(速い)
;----------------------------
;(define (fast-expt b n)
;  (cond ((= n 0) 1)
;        ((even? n) (square (fast-expt b (/ n 2))))
;        (else (* b (fast-expt b (- n 1))))))

;(define (even? n)
;  (= (remainder n 2) 0))

;----------------------------
; 反復プロセス(速い)
;----------------------------
(define (even? n)
  (= (remainder n 2) 0))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter (square b) (/ n 2) a))
          (else (expt-iter b (- n 1) (* a b)))))

;ex)
; (expt 2 5)
; (expt 2 5 1)
; (expt 2 4 2)
; (expt 2**2 2 2)
; (expt 2**3 1 2)
; (expt 2**4 0 2**5)
