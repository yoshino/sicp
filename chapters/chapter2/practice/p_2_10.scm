; コンストラクタ
(define (make-interval a b) (cons a b))

; 0をまたぐ時にメッセージを発生させる
(define (make-interval a b)
  (if (< (* a b) 0)
     (error "spanの中に0を含まないでください")
     (cons a b)))
