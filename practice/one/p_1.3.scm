(define (comparison x y z)
  (cond ((>= x y) (square x))
        ((>= x z) (square x))
        (else 0)))

(define (pick_square x y z)
  (+ (+ (comparison x y z) (comparison y x z)) (comparison z x y)))


                                              
