;; Unique
(define (unique operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((result (qeval (unique-query operands) (singleton-stream frame))))
       (if (and (not (stream-null? result))
                (stream-null? (stream-cdr result)))
           result
           the-empty-stream)))
    frame-stream))

;;; Query input :
(unique (salary ?x ?y))

;;; Query results :
=>simple-query!

;;; Query input :
(unique (salary ?x 35000))

;;; Query results :
=>simple-query!
(unique (salary (Fect Cy D) 35000))
