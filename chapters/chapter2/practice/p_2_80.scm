(put 'zero? '(scheme-number)
     (lambda (x) (= x 0)))

;; 有理数算術演算パッケージ equ? を追加
  (define (equ-zero? x)
    (= (denom x) 0))
  (put 'zero? '(rational)
       (lambda (x) (equ-zero?)))

;; 複素数算術演算パッケージに equ? を追加
  (define (equ-zero? z1 z2)
    (and (= (real-part z1) 0)
         (= (imag-part z1) 0))
  (put 'equ? '(complex)
       (lambda (z1) (equ-zero? z1)))

(define (equ-zero? x) (apply-generic 'equ-zero? x))
