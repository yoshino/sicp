;----------------------------------------
; ２次元のテーブル
;----------------------------------------
(define (make-table)
  (let ((local-table (list '*table*)))

  (define (assoc key records)
    (cond ((null? records) #f)
          ((eq? key (caar records)) (car records))
           (else (assoc key (cdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                    (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))

            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;----------------------------------------
; 一般化する
;----------------------------------------

; 2次元のテーブルはこんな感じで格納
; (*table* (number (int . 1000)) (str (country . korea)))

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup key-list)
      (define (iter keys table)
        (if (null? keys)
            (if table
                (cdr table)
                #f)
            (let ((subtable (assoc (car keys) (cdr table))))
              (if subtable
                  (iter (cdr keys) subtable)
                  #f))))
      (iter key-list local-table))

    (define (insert! key-list value)
      (define (iter keys table)
        (if (null? keys)
            (begin (set-cdr! table value)
                   'ok)
            (let ((subtable (assoc (car keys) (cdr table))))
              (if subtable
                  (iter (cdr keys) subtable)
                  (let ((new-pair (cons (car keys) '())))
                    (set-cdr! table (cons new-pair (cdr table))) ;キーが抜けていれば間のキーを作製する
                    (display new-pair)
                    (newline)
                    (display table)
                    (newline)
                    (iter (cdr keys) new-pair))))))
      (iter key-list local-table))

     (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
     dispatch))

(define t (make-table))
(define get (t 'lookup-proc))
(define put (t 'insert-proc!))
(define l (list 'num 'real 'int))

;テーブルは入れ子構造に作成される。
