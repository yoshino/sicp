(define (add_interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub_interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (make-interval a b) (cons a b))

(define (upper-bound a) (car a))
(define (lower-bound a) (cdr a))
