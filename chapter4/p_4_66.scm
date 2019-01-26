(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ? middle-manager)))
;(Webbucks Oliver)の部下のうちで部下を持つ人数分だけ、
;(Webbucks Oliver)はカウントされる

; 以下のように給料の合計値を出すようなruleを作成したい
; しかしwheelのように検索に重複がある場合は正しくaccumulrateできない
(sum
  ?amount
  (and (job ?x (computer programmer)) (salary ?x ?amount)))

; 上手く行かない例
(sum
  ?amount
  (and (wheel ?person) (salary ?person ?amount)))

(accumulation-function
  ⟨variable⟩
  ⟨query pattern⟩

; これを防ぐ方法
; 重複を無くすためにはどうすればよいか？
