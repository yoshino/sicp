(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (car (cdr vect)))

(define (add-vect vect1 vect2)
  (list (+ (xcor-vect vect1) (xcor-vect vect2)) (+ (ycor-vect vect1)) (ycor-vect vect2)))

(define (sub-vect vect1 vect2)
  (list (- (xcor-vect vect1) (xcor-vect vect2)) (- (ycor-vect vect1)) (ycor-vect vect2)))

(define (scale-vect s vect)
  (list (* s (car vect)) (* s (cdr vect))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (car (cdr f)))
(define (edge2-frame f) (car (cdr (cdr f))))

(define (frame-cord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))))))


; transform-painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-cord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))


