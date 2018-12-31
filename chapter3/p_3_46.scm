; serializer and mutex
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'require)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p))

  (define (make-mutex)
    (let ((cell (list #f)))
      (define (the-mutex m)
        (cond ((eq? m 'acquire)
               (if (test-and-set! cell)
                   (the-mutex 'acquire))) ; trueならブロックが解除されるまでループ処理
              ((eq? m 'release) (clear! cell))))
      the-mutex))

  (define (clear! cell) (set-car! cell #f))

  (define (test-and-set! cell)
    (if (car cell)
        #t
        (begin (set-car! cell #t) #f))))


;-----------------------------------------------------
; 3.46
;-----------------------------------------------------
１つのプロセスがlistをtrueにしようとしている時に、
もう１つのプロセスもsetしようとする場合、２つのプロセスに
mutexを許すことになる。

;-----------------------------------------------------
; 3.47
;-----------------------------------------------------
; 複数のmutexを許すinfomaを実装する
(define (make-semafo n)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; trueならブロックが解除されるまでループ処理
              ((eq? m 'release) (clear! cell))))
      the-mutex))
      (make-mutex)
      (error "mutex is over limit! :" n)))

(define (clear! cell) (set! cell (cdr cell))

(define (test-and-set! cell)
  (if (>= count-mutex n)
      #t
      (begin (set! cell (cons #t cell)) #f))))

; 現在のmutexで使われている個数を返す
(define (count-mutex)
  (define (loop cell counter)
    (cond
      ((null? cell) counter)
      ((if (car cell)
           (iter (cdr cell) (+ counter 1))
           (iter (cdr cell) counter 1)))))
  (loop cell 0))

; cellをリストにしないで、カウンターとかにしてintegerにすれば簡単に定義できた。。。。
