(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (queens board-size)
   (define (queen-cols k)
     (if (= k 0)
         (list empty-board)
         (filter
          (lambda (positions) (safe? k positions))
          (flatmap
           (lambda (rest-of-queens)
             (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
           (queen-cols (- k 1))))))
   (queen-cols board-size))

; 正常
(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
          (adjoin-position new-row k rest-of-queens))
        (enumerate-interval 1 board-size)))
    (queen-cols (- k 1)))

; 例えば、k=5, board-size=3の時
; (1 5) (2 5) (3 5) の３パターンがfilterでチェックされる。
; この時、呼び出されるqueen-colsは１回
; この時の処理時間をTとおく

; 遅いロジックのflatmap
(flatmap
  (lambda (new-row) ; new-rowとrest-of-queensの引数の順序が逆
    (map (lambda (rest-of-queens)
           (adjoin-position new-row k rest-of-queens))
         (queen-cols (- k 1))))
  (enumerate-interval 1 board-size))

; 例えば、k=5, board-size=3の時
; この時、呼び出されるqueen-colsは3回
; T**3
