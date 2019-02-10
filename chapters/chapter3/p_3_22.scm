; queueを局所状態を利用して定義する
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (set-front-ptr! item)
      (set! front-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty-queue?)
      (null? front-ptr))

    (define (insert-queue! item)
      (cond ((empty-queue?)
             (begin
               (set-front-ptr! item)
               (set-rear-ptr! item)))
            (else
              (begin
                (set! front-ptr (cons rear-ptr item))
                (set! rear-ptr item)))))

    (define (delete-queue!)
      (cond
        ((empty-queue?) (error "DELETE! called with an empty queue" queue))
        (else (begin
                (if (pair? front-ptr)
                    (set-front-ptr! (cdr front-ptr))
                    (set-front-ptr! '()))))))

    (define (print-queue)
      (if (not (null? front-ptr))
        (list front-ptr rear-ptr)
        '()))

    (define (dispatch m)
      (cond
        ((eq? m 'set-front-ptr) set-front-ptr)
        ((eq? m 'set-rear-ptr) set-rear-ptr)
        ((eq? m 'empty-queue?) empty-queue?)
        ((eq? m 'insert-queue!) insert-queue!)
        ((eq? m 'delete-queue!) delete-queue!)
        ((eq? m 'print-queue) print-queue)))

    dispatch))

(define q1 (make-queue))

;gosh> ((q1 'insert-queue!) 'a)
;a
;gosh> ((q1 'insert-queue!) 'b)
;b
;gosh> ((q1 'print-queue))
;((a . b) b)
;gosh> ((q1 'delete-queue!))
;b
;gosh> ((q1 'print-queue))
;(b b)
;gosh> ((q1 'delete-queue!))
;()
;gosh> ((q1 'insert-queue!) 'b)
;b
;gosh> ((q1 'print-queue))
;(b b)
