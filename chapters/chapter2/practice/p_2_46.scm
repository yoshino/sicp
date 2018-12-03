(define (make-vect x y)
  (cons x y))

; selector
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (car (cdr vect)))

;gosh> (make-vect (list 5 5) (list 1 1))
;(-4 -4)
;gosh> (xcor-vect (make-vect (list 5 5) (list 1 1)))
;-4
;gosh> (ycor-vect (make-vect (list 5 5) (list 1 1)))
;-4

(define (add-vect vect1 vect2)
  (list (+ (car vect1) (car vect2)) (+ (car (cdr vect1)) (car (cdr vect2)))))

(define (sub-vect vect1 vect2)
  (list (- (car vect1) (car vect2)) (- (car (cdr vect1)) (car (cdr vect2)))))

(define (scale-vect s vect)
  (list (* s (car vect)) (* (car (cdr vect)))))
