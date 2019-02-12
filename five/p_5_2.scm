; GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b ( remainder a b))))

(controller
  test-b
  (test (op =) (reg b) ( const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label test-b))
  gcd-done)

; Factorial
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product )
              (+ counter 1))))
  (iter 1 1))

; *const
; 1

; *register*
; n
; counter, next-counter
; product, next-product

; *op*
; >, multi, add

(controller
  ; initialize
  (assign counter (const 1))
  (assign product (const 1))
  ;
  test-n
  (test-n (op >) (reg next-counter) (reg n))
  (branch (label factorial-done))
  (assign counter (reg next-counter))
  (assign product (reg next-product))
  (assign next-product (op multi) (reg counter) (reg product))
  (assign next-counter (op add) (reg counter) (const 1))
  (goto (label test-n))
  factorial-done)
