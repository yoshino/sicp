; 動詞の解析
(define verbs '(verb studies lectures eats sleeps))

; 文法
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)    ; 主語
        (parse-verb-phrase)))  ; 動詞

; 前置詞の解析
; ex) for the cat
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

;-----------------------------------------------------------
; 動詞の解析
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase ; amb 動詞　or 動詞+前置詞
         (maybe-extend
           (list 'verb-phrase
                 verb-phrase
                 (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

; Louisの提案
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

; run with the dog.
(verb-phrase
  (verb-phrase
    (verb run)
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun dog))))

; Louisの提案の下が呼び出されれたとき
; run with dog
(verb-phrase
  (parse-verb-phrase)          ; 無限ループ
  (parse-prepositional-phrase)

