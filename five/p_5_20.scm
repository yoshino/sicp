;練習問題 5.20: free ポインタの初期値を p1 として、次の式によって生成されるリスト構造について、
;(図 5.14のように) 箱とポインタ表現とメモリベクタ表現を描け。

(define x (cons 1 2))
(define y (list x x))

;nやpは型
    | p1 p2 p3
car | n1 p1 p1
cdr | n2 p3 eo

;以下のような答えはどうなんだろう
    | p1 p2
car | n1 p1
cdr | n2 p1
