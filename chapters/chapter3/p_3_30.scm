(define (or-gate a b output) (let ((c (make-wire)) (d (make-wire))
        (e (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gete c d e)
    (inverter e output)
    'ok))

; 繰り上がり伝播加算器
; 和の結果と桁上がりを出力する

; 和の結果は排他的論理和: s
; 繰り上がりは論理積: c

; 排他的論理和
; (OR(AND(NOT(A) B))(AND A NOT(B)))

(define (ripple-carry-adder a b s c)
  (let ((x (make-wire))
        (y (make-wire))
        (not_a (make-wire))
        (not_b (make-wire)))
    ; 排他的論理和（S）
    (inverter a not_a)
    (inverter b not_b)
    (and-gate a not_b x)
    (and-gate not_a b y)
    (or-gate  x y s)
    ; 論理積
    (and-gate x y c)
    'ok))

; 時間は長い排他的論理和に依存するので、
; inverter + and + or

; これは半加算器を作っているだけ。
; SICPで定義されている全加算器を使って、
; 伝播加算器（n桁に対応したもの）を作っていくのが正解
