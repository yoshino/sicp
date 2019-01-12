(define (run-forever) (run-forever))
(define (try p)
  (if (halts? p p) (run-forever) 'halted))

(try try) を評価するときを考える

1) tryが停止するとき
run-foreverの処理が走り終わらない

2) tryが無限に走る時
(halts? p p)が#fを返すので'haltedを返す：つまり止まる

halts?は矛盾してしまうので定義できない
