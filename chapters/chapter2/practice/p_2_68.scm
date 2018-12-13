(define (memq item x)
    (cond
        ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;gosh> (print sample-tree)
;((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
;gosh> sample-message
;(0 1 1 0 0 1 0 1 0 1 1 1 0)
;gosh> (decode sample-message sample-tree)
;(A D A B B C A)

; (A D A B B C A) -> (0 1 1 0 0 1 0 1 0 1 1 1 0)
(define (encode-symbol symbol tree)
  (define (enc-iter tree)
    (if (leaf? tree)
        '()
        (if (memq symbol (symbols (left-branch tree)))
            (cons 0 (enc-iter (left-branch tree)))
            (cons 1 (enc-iter (right-branch tree))))))
  (if (memq symbol (symbols tree))
      (enc-iter tree)
      (error "Not Found symbol of " symbol)))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

gosh> (encode '(A D A B B C A) sample-tree)
(0 1 1 0 0 1 0 1 0 1 1 1 0)
