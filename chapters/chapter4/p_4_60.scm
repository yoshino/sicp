; lives-nearの実装に関して

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(lives-near ?person (Hacker Alyssa P))

; result
(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))


; この結果は条件を満たす全てのものをandが返すことにある

;(教科書より）
;単純クエリの場合と同じように、システムが複合クエリを処理する際には、パ
;ターン変数に対する代入でそのクエリを満たすものをすべて探し、それらの値
;によるクエリの具体化をすべて表示します。

; 組み合わせをユニークにするためにはfilterを検索結果にかけることで可能??

(rule (lives-near ?person-1 ?person-2)
      (uniq-pair (and (address ?person-1 (?town . ?rest-1))
                      (address ?person-2 (?town . ?rest-2))
                      (not (same ?person-1 ?person-2)))))

; uniq-pair
(rule (uniq-pair ???
