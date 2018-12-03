(define (make-vect x y)
  (cons x y))

; selector
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (car (cdr vect)))

(define (sub-vect vect1 vect2)
  (list (- (xcor-vect vect1) (xcor-vect vect2)) (- (ycor-vect vect1)) (ycor-vect vect2)))

; from here!!
(define (make-segment v1 v2) (cons v1 v2))

(define (start-segment segnment) (car segment))

(define (end-segment segnment) (cdr segment))









