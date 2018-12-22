; message passing
; duck typingに似ている気がする
(define (make-from-real-img x y)
  (define dispatch op)
  (cond ((eq? op 'real-part) x)
        ((eq? op 'imag-part) y)
        ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
        ((eq? op 'angle) (atan y x))
        (lese (error :Unkown op: "MAKE-FORM-REAL-IMAG" op))))

;2.76
(define (make-from-real-ang x y)
  (define dispatch op)
  (cond ((eq? op 'real-part) x)
        ((eq? op 'imag-part) y)
        ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
        ((eq? op 'angle) (atan y x))
        (lese (error :Unkown op: "MAKE-FORM-REAL-IMAG" op))))

; 明示的ディスパッチによるジェネリック演算 : データにタグ付けを行い分類
; データ主導スタイル : データの構造に依存：get put で内部の振る舞いには関与しない
; メッセージパッシング

; 下に行けば行くほど、データの内部に関しての知識が外部と分離されるので、
; 知識がいらないので大規模開発に向いている。
