(define nouns '(noun student professor cat class))  ; 名詞
(define verbs '(verb studies lectures eats sleeps)) ; 動詞
(define articles '(article the a))                  ; 冠詞
(define prepositions '(prep for to in by with ))    ; 前置詞

; 解析のstart
(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*)) sent))

; 文法
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)    ; 主語
        (parse-verb-phrase)))  ; 動詞

; (parse '(the cat eats))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))                  ; '(the cat eats)が空でないこと
  (require (memq (car *unparsed*) (cdr word-list)))   ; the が (cdr'(article the a))にあること
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))                ; '(the cat eats)->'(cat eats)
    (list (car word-list) found-word)))               ; (article the)

; 名詞句
(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles) ; articles '(article the a)
        (parse-word nouns)))  ; nouns    '(noun student professor cat class)

; 前置詞の解析
; ex) for the cat
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

; 動詞の解析
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase ; amb 動詞　or 動詞+前置詞
         (maybe-extend
           (list 'verb-phrase
                 verb-phrase
                 (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

; 名詞句の拡張
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase ; amb 名詞 or 名詞 + 前置詞
         (maybe-extend
           (list 'noun-phrase
                 noun-phrase
                 (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

; ex)
; (parse '(the professor lectures to the student with the cat ))

; 解釈1
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase
        (prep to)
        (simple-noun-phrase (article the) (noun student))))
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun cat)))))

; 解釈2
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase
      (prep to)
      (noun-phrase
        (simple-noun-phrase (article the) (noun student))
        (prep-phrase
          (prep with)
          (simple-noun-phrase (article the) (noun cat)))))))

;-------------------------------------------------------------
;4.45
;-------------------------------------------------------------
; The professor lectures to the student in the class with the cat
; 構造的には以下の５分類

; 教授は 猫と一緒にて教室にいる生徒に 講義する
; The professor
; (lecturres)
; (to the student in the class with the cat)

; 教授は 猫のいる教室で 生徒に講義する
; The professor
; (lecturres to the student)
; (in the class with the cat)

; 教授は猫と一緒に 教室の中で 生徒に講義する
; The professor
; lecturres to the student in the class
; with the cat

; 教授は　猫と一緒に　教室で　生徒に講義する
; The professor
; (lecturres)
; to the student
; in the class
; with the cat)

; 教授は　猫と一緒に　教室で　生徒に講義する
; The professor
; (lecturres) to the student
; in the class
; with the cat)
