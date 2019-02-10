;練習問題 4.61: 次の二つの規則は、リストの隣り合う要素を求める next-to 関係を実装している。

;rule1
; 先頭x 隣y
(rule (?x next-to ?y in (?x ?y . ?u)))

;rule2
; 再帰
(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

;以下のクエリに対する答えはどのようになるだろうか。

;問題
(?x next-to ?y in (1 (2 3) 4))

;CASE1: ?x = 1
(1 next-to ?y in (1 (2 3) 4))
; ?y (2 3)

;CASE2: ?x = (2 3)
((2 3) next-to ?y in (1 (2 3) 4))
;((2 3) next-to ?y in (2 3) 4)) ;rule2をてきようしてrule1を使えるよになった
; ?y 4

;問題
(?x next-to 1 in (2 1 3 1))

;CASE1: ?x = 2
;(2 next-to 1 in (2 1 3 1))
;rule1 ok

;CASE2: ?x = 1
; false

;CASE2: ?x = 3
;(3 next-to 1 in (2 1 3 1))
;(3 next-to 1 in (1 3 1))
;(3 next-to 1 in (3 1))
;ok



