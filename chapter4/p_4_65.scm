(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ? middle-manager)))

(wheel who?) ;; ?personに?whoを束縛

(and

;ここで ?middle-managerにpersonの部下Aを束縛する
(supervisor ?middle-manager ?who)

;次にこの条件で?xを束縛する
(supervisor ?x ?middle-manager)))

;(Webbucks Oliver)の部下のうちで部下を持つ人数分だけ、
;(Webbucks Oliver)はカウントされる
