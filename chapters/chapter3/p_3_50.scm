;ストリームに関して

;ストリーム処理の力の一部は、プログラムの中で実際に出来事が起こる順番を考えなくていいというところから来ています。
;残念ながら、代入があると、このような考え方はできません。代入を使うときには、時間と変化について考えることが避けられないからです。

;プログラムはあたかも完全な列を扱っているかのように書くのですが、ストリームの実装は、ストリームの構築とストリームの使用が自動的かつ透過的に組み合わせられるように設計します。

(stream-car ( cons-stream x y))
(stream-cdr ( cons-stream x y))


;データ抽象化としては、ストリームはリストと同じです。違うのは要素が評価されるタイミングです。通常のリストでは、
;car も cdr も構築時に評価されます。ストリームでは、cdr は選択時に評価されます。

; list: list-ref map for-eachのストリーム版
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref  stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

; ストリームの中身をみる便利な関数も作成できます
(define (display-stream s)
  (stream-for-each display-line s))
(define ( display-line x) ( newline ) ( display x))

(define ( stream-car stream) (car stream))
(define ( stream-cdr stream) (force (cdr stream)))

; これを実装していく
(stream-car
  (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

; filter
; 遅延評価を行いフィルタリングを行う

; ここでいう遅延評価とは１つずつカウントアプしていく、其のたびごとに評価するということである。
; 例えば、素数を２つ見つける場合、見つけた時点でプログラムを終えることが可能。
; streamにはそれ以上のでデータがあるが、実際に使われるのは遅延評価される部分だけ。
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred ( stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                        pred
                        (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (force delayed-object) (delayed-object))

(define (memo-proc proc)
  (let (( already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

;-----------------------------------------------------
; 3.50
;-----------------------------------------------------
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

; 複数のstreamに対応したmap
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (begin
        (apply proc (map (stream-car argstreams)))
        (apply stream-map
               (cons proc (map (stream-cdr argstreams)))))))


;-----------------------------------------------------
; 3.51
;-----------------------------------------------------
(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

; インタープリターへの表示
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref  stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred ( stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                        pred
                        (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; 以下を実行した時
(stream-ref x 5) ; 0 1 2 3 4
(stream-ref x 7) ; 0 1 2 3 4 

;-----------------------------------------------------
; 3.52
;-----------------------------------------------------
(define sum 0)
(define (accum x) (set! sum (+ x sum )) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter
            (lambda (x) (= (remainder x 5) 0))
                         seq))

(stream-ref y 7)
; (stream-filter even? seq)
; 2 4 6 8 10 12 14 16 18 20 とストリームが流れてきて、
; accum: 足し合わせていき、110
; (stream-ref y 7) の結果は８番目の18

(display-stream z)
; (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))は、
; 5 10 15 20 とストリームに流れてきて、
; sum: 50
; (display-stream z)の結果は、5 10 15 20

