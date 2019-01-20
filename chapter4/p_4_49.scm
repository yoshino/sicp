; 問題を同じ品詞の中で別のものに入れ替えるのにはどうしたら良いか?
; という問に置き換える

(define (parse-word word-list)
  (require (not (null? *unparsed*)))                   ; '(the cat eats)が空でないこと
  (require (memq (car *unparsed*) (cdr word-list)))    ; the が (cdr'(article the a))にあること
  ; 文章の構造は維持して単語だけ入れ替える
  (let ((found-word (pick-rand (car word-list) *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))                 ; '(the cat eats)->'(cat eats)
    (list (car word-list) found-word)))                ; (article the)

; randomに品詞のリストから１つとる
; randomが使えれば解決,,
(define (pick-rand hinshi w-lst)
  ; todo!)
