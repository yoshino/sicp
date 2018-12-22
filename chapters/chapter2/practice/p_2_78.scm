(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contens datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
(let
                ((type-tags (map type-tag args)))
                            (let ((proc (get op type-tags)))
                                              (if proc
                                                                    (apply proc (map contents args))
                                                                                    (error
                                                                                                          "No method for these types: APPLY-GENERIC"
                                                                                                                              (list op type-tags ))))))


; ジェネリックシステムがschemeの内部のシステムを利用できるようにする

; 通常(ordinary)の算術演算パッケージに equ? を追加
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))

;; 有理数算術演算パッケージ equ? を追加
  (define (equ-rat? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rat? x y)))

;; 複素数算術演算パッケージに equ? を追加
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ-complex? z1 z2)))

(define (equ? x y) (apply-generic 'equ? x y))
