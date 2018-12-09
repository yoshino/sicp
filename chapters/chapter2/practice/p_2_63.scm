(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((= x (entry set)) true)
    ((< x (entry set))
     (element-of-set? x (left-branch set)))
    ((> x (entry set))
     (element-of-set? x (right-branch set)))

(define (adjoin-set x set)
  (cond
    ((null? set) (make-tree x '() '()))
    ((= x (entry set)) set)
    ((< x (entry set))
     (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
    ((> x (entry set))
     (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))))

(define (tree->list1 tree)
  (if (null? tree)
    '()
    (append (tree->list1 (left-branch tree))
            (cons (entry tree)
                  (tree->list1
                    (right-branch tree))))))

; '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

; append左の枝＋右の枝するので速い
(append (tree->list1 (3 (1 () ()) (5 () ())))
        (cons 7
              (tree->list1
                ((9 () (11 () ()))))))))

(define (tree->list2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list
                            (right-branch tree)
                            result-list))))))

; 加えていくの１つの要素ずつなので遅い
(copy-to-list (3 (1 () ()) (5 () ()))
              (cons 7
                    (copy-to-list
                      (9 () (11 () ()))
                      '()))))))

(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

