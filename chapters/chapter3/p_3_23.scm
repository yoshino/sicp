;--------------------------------------------------------
; 3.23: 終端に加えて先端にも追加できるdeque
;--------------------------------------------------------
(define (make-deque)
  (define (debug arg)
    (display arg)
    (newline))

  (define front '())
  (define rear '())

  (define (set-front! item) (set! front item))
  (define (set-rear! item) (set! rear item))
  (define (empty-deque?) (null? front))

  (define (insert-front! item)
    (let ((new-front (cons (cons item '()) front)))
      (cond ((empty-deque?) (set-front! new-front)
                            (set-rear! new-front)
                            dispatch)
            (else
              (debug front)
              (debug new-front)
              (set-cdr! (car front) new-front)
              (debug front)
              (debug new-front)
              (set-front! new-front)
              dispatch))))

  (define (insert-rear! item)
    (let ((new-rear (cons (cons item rear) '())))
      (cond ((empty-deque?) (set-front! new-rear)
                            (set-rear! new-rear)
                            dispatch)
            (else (set-cdr! rear new-rear)
                  (set-rear! new-rear)
                  dispatch))))

  (define (delete-front!)
    (cond ((empty-deque?) (error "DELETE-FRONT! called on empty queue" front))
          (else (set-front! (cdr front))
                (unless (empty-deque?)
                  (set-cdr! (car front) '()))
                dispatch)))

  (define (delete-rear!)
    (cond ((empty-deque?) (error "DELETE-REAR! called on empty queue" rear))
          (else (set-rear! (cdar rear))
                (if (null? rear)
                    (set-front! '())
                    (set-cdr! rear '()))
                dispatch)))

  (define (print-deque)
    (define (print-start) (display "("))
    (define (print-end) (display ")") (newline))
    (print-start)
    (let print-next ((next front))
      (cond ((null? next) (print-end))
            ((null? (cdr next)) (display (caar next))
                                (print-end))
            (else (display (caar next))
                  (display " ")
                  (print-next (cdr next))))))

  (define (dispatch m)
    (cond ((eq? m 'insert-front!) insert-front!)
          ((eq? m 'insert-rear!) insert-rear!)
          ((eq? m 'delete-front!) delete-front!)
          ((eq? m 'delete-rear!) delete-rear!)
          ((eq? m 'front) front)
          ((eq? m 'rear) rear)
          ((eq? m 'print) (print-deque))
          (else (error "DEQUEUE -- Unknown instruction" m))))
  dispatch)

(define dq (make-deque))

;gosh> (((dq 'insert-rear!) 'a) 'print)
;(a)
;gosh> (((dq 'insert-rear!) 'b) 'print)
;(a b)
;gosh> (((dq 'insert-front!) 'c) 'print)
;(c a b)
;gosh> (((dq 'insert-front!) 'd) 'print)
;(d c a b)
;gosh> (((dq 'delete-front!)) 'print)
;(c a b)
;gosh> (((dq 'delete-rear!)) 'print)
;(c a)

; frontを使ってa b cを追加しても
;gosh> ((dq 'insert-front!) 'a)
;gosh> ((dq 'insert-fornt!) 'b)
;gosh> ((dq 'insert-front!) 'c)

; rearを使ってa b cを追加しても
;gosh> ((dq 'insert-rear!) 'a)
;gosh> ((dq 'insert-rear!) 'b)
;gosh> ((dq 'insert-rear!) 'c)

; front rearの値は変わらない
;gosh> (dq 'front)
;#0=((c) . #1=((b . #0#) (a . #1#)))
;gosh> (dq 'rear)
;#0=((a . #1=((b (c) . #1#) . #0#)))

; front は常に先頭のaにポイントが指し示し、
; rear は常に一番後ろのcにポイントがさされさかのぼるようにリストを保持している.

; front と rearを織り交ぜても同じ。
;gosh> ((dq 'insert-front!) 'a)
;gosh> ((dq 'insert-front!) 'b)
;gosh> ((dq 'insert-rear!) 'c)

;gosh> (dq 'front)
;#0=((b) . #1=((a . #0#) (c . #1#)))
;gosh> (dq 'rear)
;#0=((c . #1=((a (b) . #1#) . #0#)))
