(define (front-ptr queue) (car queue))
(define (rear-ptr  queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (cond
    ((empty-queue? queue) (error "DELETE! called with an empty queue" queue))
    (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;------------------------------------------------------------------
;3.21
;------------------------------------------------------------------
(define q1 (make-queue))

; front-ptrがリストに対してのポインタを持っているので、
; 表示のためには表示用のメソッドが必要
(define (print-queue q)
  (if (not (null? (car q)))
    (list (car (front-ptr q)) (car (rear-ptr q)))
    '()))

;gosh> (print-queue q1)
;()
;gosh> (insert-queue! q1 'a)
;((a) a)
;gosh> (print-queue q1)
;(a a)
;gosh> (insert-queue! q1 'b)
;((a b) b)
;gosh> (print-queue q1)
;(a b)
;gosh> (delete-queue! q1)
;((b) b)
;gosh> (print-queue q1)
;(b b)
;gosh> (delete-queue! q1)
;(() b)
;gosh> (print-queue q1)
;()
