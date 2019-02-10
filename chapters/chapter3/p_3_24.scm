;----------------------------------------
; 一次元のテーブル
;----------------------------------------
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record) ; 先頭にはダミーデータが入っていると前提
        false)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
         (else (assoc key (cdr records)))))

; assoc は member と同様に Lisp の伝統的な関数です。
; assoc は連想リスト a-list から obj と等しいキーを探します。見つからない場合は #f を返します。

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value)
                      (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define t (make-table))

;gosh> (insert! 'a 1 t)
;gosh> (insert! 'b 2 t)
;gosh> t
;(*table* (b . 2) (a . 1))

;gosh> (insert! 'a 10 t)
;gosh> t
;(*table* (b . 2) (a . 10))

;----------------------------------------
; ２次元のテーブル
;----------------------------------------
(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
                (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))

        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))

  'ok)

(define (make-table)
  (list '*table*))

(define t (make-table))

;gosh> (insert! 'letters 'a 97 t)
;gosh> (insert! 'letters 'b 98 t)
;gosh> (insert! 'math '+ 43 t)
;gosh> (insert! 'math '- 45 t)
;gosh> (insert! 'math '* 42 t)
;gosh> t
;(*table* (math (* . 42) (- . 45) (+ . 43)) (letters (b . 98) (a . 97)))

;gosh> (lookup 'letters 'a t)
;97

;----------------------------------------
; 局所状態をもつテーブル
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

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;gosh> (put 'math '+ 10)
;gosh> (put 'math '- 20)
;gosh> (put 'letters 'a 12)
;gosh> (get 'math '+)
;10

;-------------------------------------------------------------------
; 3.24
;-------------------------------------------------------------------
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
         (else (assoc key (cdr records)))))

; assocの中で使われているeq? のかわりにsame-key?を実装する
(define (same-key? key-1 key-2)
  (if (and (number? key-1) (number? key-2))
      (if (and (< key-1 100) (< key-2 100)) ; keyが数字で100以下なら同じkeyとみなす
          #t
          (= key-1 key-2))
      (eq? key-1 key-2)))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))

  (define (assoc key records)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
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

    (define (print-table)
      (display local-table)
      (newline))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print) print-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table same-key?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print (operation-table 'print))

; 200以下の数字は同じキーとみなされているので、上書きされている。
;gosh> (put 'num 1 'one)
;gosh> (put 'num 2 'two)
;gosh> (get 'num 1)
;two
;gosh> (get 'num 2)
;two

;gosh> (put 'num 200 'twohundred)
;gosh> (get 'num 200)
;twohundred
