(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; 行列の内積
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define r1 (list 1 2 3))
(define r2 (list 4 5 6))
(define r3 (list 7 8 9))
(define l (list r1 r2 r3))

; gosh> (dot-product r1 r1)
; 14
; gosh> (dot-product r1 r2)
; 32

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define l1 (list (list 1 2 3) (list 4 5 6)))
(define l2 (list 1 2 3))

;gosh> (matrix-*-vector l1 l2)
;(14 32)

(define r1 (list 1 2 3))
(define r2 (list 4 5 6))
(define r3 (list 7 8 9))
(define l (list r1 r2 r3))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    ()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (transpose mat)
  (accumulate-n cons () mat))

;gosh> (transpose l)
;((1 4 7)
; (2 5 8)
; (3 6 9))
;gosh> l
;((1 2 3)
; (4 5 6)
; (7 8 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define r1 (list 1 2 3))
(define r2 (list 4 5 6))
(define r3 (list 7 8 9))
(define l (list r1 r2 r3))

;gosh> (matrix-*-matrix l l)
;((30 36 42) (66 81 96) (102 126 150))

