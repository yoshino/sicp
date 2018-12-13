(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records))) (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

; 上記のものがランダムな集合
; これが二進木で構造化されている場合にはどうなるか？
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (enrty set-of-records))) (entry set-of-records))
        ((> given-key (key (entry set-of-records))) (lookup given-key (right-branch set-of-records)))
        ((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))))
