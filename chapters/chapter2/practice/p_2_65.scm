(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (entry set)) #t)
    ((< x (entry set))
     (element-of-set? x (left-branch set)))
    ((> x (entry set))
     (element-of-set? x (right-branch set)))

; tree構造を利用した以下の集合の検索アルゴリズムはこんな感じかと思った。
; 積集合
(define (interection-set tree1 tree2)
  (cond
    ((or (null? tree1) (null? tree2)) '())
    ((= (entry tree1) (entry tree2))
     (cons (entry tree1)
           (interection-set (left-branch tree1) (left-branch tree2))
           (interection-set (right-branch tree1) (right-branch tree2))))
    ((> (entry tree1) (entry tree2))
     (cons (interection-set (left-branch tree1) (right-branch tree2))
    ((< (entry tree1) (entry tree2))
     (cons (interection-set (left-branch tree2) (right-branch tree1))))))))

; 和集合
(define (union-set tree1 tree2)
    ((null? tree1) tree2)
    ((null? tree2) tree1)
    ((= (entry tree1) (entry tree2))
     (cons
       (union-set (left-branch tree1) (left-branch tree2))
       (union-set (right-branch tree1) (right-branch tree2))))
    ((> (entry tree1) (entry tree2))
     (cons
       (entry tree1)
       (entry tree2)
       (right-branch tree1)
       (left-branch tree2)
       (union-set (left-branch tree1) (right-branch tree2))))
    ((< (entry tree1) (entry tree2))
       (entry tree2)
       (entry tree1)
       (right-branch tree2)
       (left-branch tree1)
       (union-set (left-branch tree2) (right-branch tree1))))

;問題の意図は少し違って、tree->list(ここで元からあったlistへの処理->treeみたいなことを期待して
;いたらしい

; helper methodを単純に定義すればよく。

; tree->listが必要
(define (tree->list tree)
  (if (null? tree)
    '()
    (append (tree->list1 (left-branch tree))
            (cons (entry tree)
                  (tree->list1
                    (right-branch tree))))))

(define (union-set tree1 tree2)
  (define (union-list set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((= (car set1) (car set2))
           (cons (car set1) (union-list (cdr set1) (cdr set2))))
          ((< (car set1) (car set2))
           (cons (car set1) (union-list (cdr set1) set2)))
          (else (cons (car set2) (union-list set1 (cdr set2))))))
  (list->tree (union-list (tree->list-2 tree1)
                          (tree->list-2 tree2))))

(define (intersection-set tree1 tree2)
  (define (intersection-list set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1
                       (intersection-list (cdr set1)
                                          (cdr set2))))
                ((< x1 x2)
                 (intersection-list (cdr set1) set2))
                ((< x2 x1)
                 (intersection-list set1 (cdr set2)))))))
  (list->tree (intersection-list (tree->list-2 tree1)
                                 (tree->list-2 tree2))))
