(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

;gosh> (factorial 5)
;*** ERROR: operation * is not defined between 2 and #<undef>

;schemeは適用順序で評価をおこなう
;以下のように展開されて(factorial 1)がundefinedになってしまう
;(* 5 (* 4 (* 3 (* 2 (* (factorial 1))))))
;本来は(factorial 1) ; 1 となるべきところが、
;(factorial 1): (* 1 (factorial 0)) ; undef を返してしまい無限ループに入るため

;正規順序(遅延評価)で評価を行う場合どうなるであろうか？
;正規順序の場合はunless節を評価した後1を返すので上記のようなエラーを起こさない。

