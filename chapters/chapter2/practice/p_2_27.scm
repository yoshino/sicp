; reverse
(define (reverse items)
  (reverse-iter items (list)))

(define (last-pair items)
  (list-ref items (- (length items) 1)))

(define (reverse-iter items result)
  (cond
    ((not (list? items)) items)
    ((null? items) result)
    (else (reverse-iter (cdr items) (cons (car items) result)))))

; count-leaves
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; deep-reverse
(define (deep-reverse items)
  (cond
    ((= (length items) (count-leaves items)) (reverse items))
    (else (cons (deep-reverse (car items)) (deep-reverse (cdr items))))))
