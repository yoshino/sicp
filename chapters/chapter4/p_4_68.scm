; append
(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z)) ; 先頭に?uが入って?vは二番目に来る
(append-to-form ?v ?y ?z))

; 逆順
(reverse (1 2 3) ?x)
(reverse ?x (1 2 3))

; 4.62で定義した(last-pair ?list ?x)を使えるものとする
(rule (reverse ?ori ?rev)
      (and (last-pair ?ori ?last)
           (append-to-form ?last ?ori)))
(rule (reverse (?ori-first . ?ori-rest) (?rev-first . ?rev-rest))
      (and (last-pair (?ori-first . ?ori-rest) ?last)
           (append-to-form ?last (reverse ?ori-rest ?rev-rest))))



; 正解
; 正順のfirstを逆順のappendの時に使うことが味噌
(assert! (rule (reverse () ())))
(assert! (rule (reverse ?x ?y)
              (and (append-to-form (?first) ?rest ?x)
                   (append-to-form ?rev-rest (?first) ?y)
                   (reverse ?rest ?rev-rest))))
