; 予定表の実装
(define (make-time-segmant time queue)
  (cons time queue))
(define (sigment-time s) (car s))
(define (sigment-queue s) (cdr s))

; 予定表は１次元のテーブル
; 時間の前後順に並んでいる
; 予定表の先頭には現在時刻=最後にアクションを実行した時刻
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (first-segment agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null (segments agenda)))

; 時間の前後順に並ぶので追加する時はそこを意識して、
(define (add-to-agenda! time action agenda)
  ; 直近のsagmentよりも前の時刻か?
  (define (belongs-to-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  ;
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segmant time q)))
  ;
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        ; timeが同じであればactionを上書きする
        (insert-queue! (segment-time (car segments)) action)
        (let ((rest (cdr segments))
              (if (belongs-to-before? rest)
                  ; 時刻より前であればsegmentsの前に追加する
                  (set-cdr! segments (cons (make-new-time-segment time action) (cdr segments)))
                  ; そうでなければ後ろの時刻を探していく
                  (add-to-segments! rest)))))
    (let ((segments (segments agenda)))
      (if (belongs-to-before? segments)
          (set-segments! agenda (cons make-new-time-segment tim action) segments)
          (add-to-segments! segments))))
  ;
  (define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
      (delete-queue! q)
      ; qが空ならagendaから削除する
      (if (empty-queue? q)
          (set-segments! agenda (rest-segments agenda)))))
  ; 項目を取り出すたびに最新の時刻を更新しなければいけない
  (define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "Agenda is empty: FIRST-AGENDA-ITEM")
        (let ((first-seg (first-segment agenda)))
          (set-current-time! agenda (segment-time first-seg))
          (front-queue (segment-queue first-segment))))))



; 3.32
; たとえば１つの時間区分Qにおいて、登録される手続きの結果の２つのWIREのを並べてみる。

; 通常、前から後ろへと処理が進む。
; (0, 1) -> (1, 1にする処理) -> (ANDゲートは１を出力する）->  (1,0)

; これを後ろからと処理をすすめると、ANDゲートは０を出力する。
; つまり時間軸において対照性がないので一定方向から処理をすすめる必要がある。
