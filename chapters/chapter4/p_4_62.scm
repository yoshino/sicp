;練習問題 4.62: 練習問題 2.17の last-pair 演算を実装する規則を定義せよ。
;これは、空でないリストの最後の要素を含むリストを返すものである。
;(last-pair (3) ?x), (last-pair (1 2 3) ?x),
;(last-pair (2 ?x) (3)) のようなクエリに対して、
;それらの規則をチェックせよ。それらの規則は (last-pair ?x (3)) のような
;クエリに対して正しく動作するだろうか。

;; 規則でのlast-pair
; rule1: pairが１つのとき
(assert! (rule (last-pair (?x . ()) (?x))))

; rule2
; 任意のx, y, zについて, yがlast-pair zであるなら
; 再帰？
(assert! (rule (last-pair (?x . ?y) ?z)
               (last-pair ?y ?z)))


(last-pair (?x . ()) (?x)

(last-pair (3) ?x)
;rule1より
; 3

(last-pair (1 2 3) ?x)
;rule2がループ
;(1 2 3) ?x
;(2 3) ?x
;(3) ?x ; rule1が適用

(last-pair (2 ?x) (3))
;(last-pair ?x(単独ペア） (3)) rule1を適用

(last-pair ?x (3))
;(cdr ?x) => 単独ペアに定まらない：ルール１を適用できず無限ループ
