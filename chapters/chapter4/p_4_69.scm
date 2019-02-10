((great grandson) Adam Irad) ; Adamのひ孫(g)はIrad
((great great grandson) Adam Irad) ; Adamのひひ孫(gg)はIrad
((great great great grandson) Adam Irad) ; Adamのひひひ孫(ggg)はIrad

(rule ((great ?grandson) ?gf ?gs)
      (and (son ?gf ?son)
           (son ?son ?gs)))
(rule ((great . ?grandson-rest) ?gf ?gs)
      (son ?gf (great ?grandson-rest ?son ?gs)))








