;A
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
   (set! THE-ASSERTIONS ;データベース内のすべての表明のストリームに格納される
     (stream-cons assertion old-assertions))
   'ok))

;B 4.70で示された誤った実装
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
    (stream-cons assertion THE-ASSERTIONS))
  'ok)

Bに関して
  (set! THE-ASSERTIONS
    (stream-cons assertion (評価) THE-ASSERTIONS (delayで未評価)))
なのでset!した時点で未評価の THE-ASSERTIONS がconsされてしまう（あるいはできない）

