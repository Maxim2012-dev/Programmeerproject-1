;;          -----------
;; --------  Level ADT  ---------
;;          -----------

(load "alienvloot-adt.rkt")
(#%require srfi/27)

(define (maak-level aantal-cellen-breedte aantal-cellen-hoogte)
  (let* ((raket-start-positie
          (maak-positie raket-start-x raket-start-y))
         (raket (maak-raket raket-start-positie))
         (alienvloot (maak-alienvloot))
         (kogels (maak-kogels-adt))
         (score (maak-score))
         (alien-schiettijd 0)
         (game-over-tijd 0)
         (game-over? #f)
         (vloot-tijd 0)
         (kogel-tijd 0))


    ;; --------------- BEWEEG - OPERATIES ---------------
    
    ;; beweeg-raket! : symbol -> /
    (define (beweeg-raket! toets)
      (cond ((eq? toets 'left)
             ((raket 'beweeg!) 'links))
            ((eq? toets 'right)
             ((raket 'beweeg!) 'rechts))))
      

    ;; beweeg-vloot! : / -> /
    (define (beweeg-vloot!)
      (if (>= vloot-tijd snelheid-vloot)
          (begin
            ((alienvloot 'beweeg))
            (set! vloot-tijd 0))))


    ;; beweeg-kogel! : / -> /
    (define (beweeg-kogels!)
      (if (and (not (null? (kogels 'kogels-lijst)))
               (>= kogel-tijd snelheid-kogel))
          (begin
            ((kogels 'voor-alle-kogels) roep-beweeg-op)
            (set! kogel-tijd 0))))


    ;; roep-beweeg-op : kogel-adt -> /
    (define (roep-beweeg-op kogel-adt)
      (let* ((y ((kogel-adt 'positie) 'y))
             (raakt-rand? (((kogel-adt 'positie) 'rand-verticaal?)))
             (type-kogel (kogel-adt 'type)))
        (if (not raakt-rand?)
            ((kogel-adt 'beweeg!) type-kogel)
            ((kogels 'verwijder-kogel!) kogel-adt))))
    


    ;; --------------- SCHIET - OPERATIES ---------------
    

    ;; schiet-raketkogel! : symbol -> /
    (define (schiet-raketkogel! toets)
      (if (eq? toets '#\space)
          (let ((nieuwe_kogel (maak-kogel (maak-positie ((raket 'positie) 'x)
                                                        ((raket 'positie) 'y))
                                          'raket)))
            ((kogels 'voeg-kogel-toe!) nieuwe_kogel))))

    
    ;; schiet-alienkogel! : / -> /
    (define (schiet-alienkogel!)
      (if (>= alien-schiettijd delay-alienschot)
          ; willekeurige alien uit het vloot kiezen
          (let* ((alien-matrix (alienvloot 'schepen))
                 (inner-vector (vector-ref alien-matrix (random-integer aantal-rijen-aliens)))
                 (schietende-alien (vector-ref inner-vector (random-integer aantal-aliens-per-rij)))
                 (nieuwe_kogel (maak-kogel (maak-positie ((schietende-alien 'positie) 'x)
                                                         ((schietende-alien 'positie) 'y))
                                           'alien)))
            ((kogels 'voeg-kogel-toe!) nieuwe_kogel)
            (set! alien-schiettijd 0))))
    
    

    ;; Dit is de procedure die constant checkt of één van de kogels
    ;; ofwel een alien ofwel de raket raakt.
    ;; check-kogels-geraakt : Teken-adt -> /
    (define (check-kogels-geraakt teken-adt)
      (let ((kogels-lijst (kogels 'kogels-lijst)))
        ; itereren over alle kogels
        (define (iter kogels-lijst)
          (if (not (null? kogels-lijst))
              (let* ((kogel (car kogels-lijst))
                     (raket-kogel? (eq? (kogel 'type) 'raket)))
                (if raket-kogel?
                    ; Als raket-kogel? dan...
                    (begin ((alienvloot 'voor-alle-schepen)
                            (lambda (alien)
                              ; kogel van RAKET raakt een alien + alien heeft 1 leven
                              (cond ((and (((alien 'positie) 'gelijk?) (kogel 'positie))
                                          (eq? (kogel 'type) 'raket)
                                          (= (alien 'levens) 1))
                                     (bepaal-score! alien teken-adt)
                                     (verwijder-alienschip! alien)
                                     (verwijder-kogel! kogel)
                                     ((teken-adt 'verwijder-kogel!) kogel))
                                    ; kogel van RAKET raakt een alien + alien heeft meer dan 1 leven
                                    ((and (((alien 'positie) 'gelijk?) (kogel 'positie))
                                          (eq? (kogel 'type) 'raket))
                                     ((alien 'levens!) (- (alien 'levens) 1))
                                     (verwijder-kogel! kogel)
                                     ((teken-adt 'verwijder-kogel!) kogel)))))
                           (iter (cdr kogels-lijst)))
                    ; Anders doe dit...
                    ; kogel van ALIEN raakt de raket + raket heeft 1 leven
                    (cond ((and (((raket 'positie) 'gelijk?) (kogel 'positie))
                                (= (raket 'levens) 1))
                           (set! game-over-tijd 0)
                           (set! game-over? #t))
                          ; kogel van ALIEN raakt de raket + raket heeft meer dan 1 leven
                          ((((raket 'positie) 'gelijk?) (kogel 'positie))
                           ((raket 'verminder-levens!))
                           ((kogels 'verwijder-kogel!) kogel)
                           ((teken-adt 'verwijder-kogel!) kogel)
                           ((teken-adt 'teken-levens) raket)
                           (iter (cdr kogels-lijst)))))
                (iter (cdr kogels-lijst)))))
        (iter kogels-lijst)))


    ;; --------------- SCORE - OPERATIES ---------------


    ; vergelijkt huidige met hoogste score en past indien nodig aan
    ; vergelijk-met-hoogste! Teken-adt -> /
    (define (vergelijk-met-hoogste! teken-adt)
      (let ((huidige-score (score 'huidige-score)))
        (if ((score 'meer-dan-hoogste?))
            (begin ((score 'verander-hoogste!) huidige-score)
                   ((teken-adt 'teken-hoogste-score) score)))))


    ; score wordt bepaald op basis van de alien die werd neergeschoten
    ; bepaal-score : Alien -> /
    (define (bepaal-score! alien teken-adt)
      (let ((soort-alien (alien 'kleur)))
        (cond ((eq? soort-alien 'blauw) ((score 'verhoog-score!) 5))
              ((eq? soort-alien 'geel) ((score 'verhoog-score!) 10))
              ((eq? soort-alien 'paars) ((score 'verhoog-score!) 15)))
        ((teken-adt 'teken-huidige-score) score)))


    ;; --------------- VERWIJDER - FUNCTIES ---------------
    

    ; individueel alienschip verwijderen van scherm
    (define (verwijder-alienschip! alienschip-adt)
      ((alienvloot 'verwijder-schip!) alienschip-adt))
    

    ; individuele kogel verwijderen van scherm
    (define (verwijder-kogel! kogel-adt)
      ((kogels 'verwijder-kogel!) kogel-adt))



    ;; --------------- GAME-OVER ---------------


    ; Als vloot bij de raket is dan : game-over? -> true
    ; check-vloot-onderkant : / -> /
    (define (check-vloot-onderkant)
      (let ((onderkant? (alienvloot 'onderkant-geraakt?)))
        (if onderkant?
            (begin
              (set! game-over-tijd 0)
              (set! game-over? #t)))))
    
    
    ;; maak-nieuw-spel! : / -> /
    (define (maak-nieuw-spel! teken-adt)
      (if (and game-over?
               (> game-over-tijd game-over-delay))
          (begin (set! game-over? #f)
          
                 ; alle elementen resetten (vloot, levens, raket en score)

                 ; vloot initialiseren met nieuwe aliens
                 ((teken-adt 'verwijder-vloot!) alienvloot)
                 ((alienvloot 'vul-vloot!))

                 ; levens van raket terug op 5 zetten
                 ((raket 'reset-levens!))
                 ((teken-adt 'teken-levens) raket)

                 ; positie van raket op startpositie
                 ((raket 'positie!) raket-start-positie)

                 ; vergelijken met record + huidige resetten
                 (vergelijk-met-hoogste! teken-adt)
                 ((score 'reset-score!))
                 ((teken-adt 'teken-huidige-score) score)

                 (display game-over?))))    


    ;; --------------- CALLBACKS ---------------
    

    ;; update! : number -> /
    (define (update! tijdsverschil teken-adt)
      (set! game-over-tijd (+ game-over-tijd tijdsverschil))
      (set! vloot-tijd (+ vloot-tijd tijdsverschil))
      (set! kogel-tijd (+ kogel-tijd tijdsverschil))
      (set! alien-schiettijd (+ alien-schiettijd tijdsverschil))
      (maak-nieuw-spel! teken-adt)
      ; Zolang niet game-over? blijf dit doen...
      (if (not game-over?)
          (begin (display "yes")(beweeg-vloot!)
                 (beweeg-kogels!)
                 (schiet-alienkogel!)
                 (check-kogels-geraakt teken-adt)
                 (check-vloot-onderkant))))
    
    
    ;; toets! : any -> /
    (define (toets! toets)
      (if (not game-over?)
          (begin (beweeg-raket! toets)
                 (schiet-raketkogel! toets))))
    
    
    ; Dispatch
    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'toets!) toets!)
            ((eq? msg 'raket) raket)
            ((eq? msg 'kogels) kogels)
            ((eq? msg 'alienvloot) alienvloot)
            (else (display "ongeldige boodschap"))))

    dispatch-level))
            

    
