;;          -----------
;; --------  Level ADT  ---------
;;          -----------

(load "alienvloot-adt.rkt")
(#%require srfi/27)

(define (maak-level aantal-cellen-breedte aantal-cellen-hoogte teken-adt)
  (let* ((raket-start-positie
          (maak-positie raket-start-x raket-start-y))
         (raket (maak-raket raket-start-positie))
         (alienvloot (maak-alienvloot))
         (kogels (maak-kogels-adt))
         (score (maak-score))
         (alien-schiettijd 0)
         (volgend-level? #f)
         (game-over-tijd 0)
         (game-over? #f)
         (power-up #f)
         (power-up-tijd 0)
         (power-up-duur 0)
         (power-up-trigger #f)
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


    ;; beweeg-power-up! : / -> /
    (define (beweeg-power-up!)
      (if (and power-up
               (>= power-up-tijd snelheid-power-up))
          (let* ((y ((power-up 'positie) 'y))
                 (raakt-rand? (((power-up 'positie) 'rand-verticaal?))))
            (if (not raakt-rand?)
                (begin ((power-up 'beweeg!))
                       (set! power-up-tijd 0))
                (set! power-up #f)))))


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
    ;; check-kogels-geraakt : / -> /
    (define (check-kogels-geraakt)
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
                                          (= (alien 'levens) 1))
                                     ((alienvloot 'verhoog-vernietigde-schepen!))
                                     ; checken voor power-up
                                    (if (= (alienvloot 'aantal-vernietigde-schepen) aliens-power-up)
                                        (creëer-power-up! alien))
                                     (bepaal-score! alien)
                                     (verwijder-alienschip! alien)
                                     (verwijder-kogel! kogel)
                                     ((teken-adt 'verwijder-kogel!) kogel))
                                    ; kogel van RAKET raakt een alien + alien heeft meer dan 1 leven
                                    ((and (((alien 'positie) 'gelijk?) (kogel 'positie))
                                          (> (alien 'levens) 1))
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
    

    ;; --------------- POWER-UP - OPERATIES ---------------
    

    ;; ----------> Validatie <----------
    
    ; om de 10 vernietigde aliens een nieuwe power-up aanmaken
    ; op de positie van de laatst vernietigde alien
    ; creëer-power-up : / -> /
    (define (creëer-power-up! alien)
      (let* ((pos-x ((alien 'positie) 'x))
             (pos-y ((alien 'positie) 'y)))
        (set! power-up (maak-power-up (maak-positie pos-x pos-y)))
        ((alienvloot 'reset-aantal-vernietigde-schepen!))))

    ; Wanneer op de tab-toets wordt gedrukt
    ; activeer-power-up! : symbol -> /
    (define (activeer-power-up! toets)
      (if (and power-up-trigger
               (eq? toets '#\tab))
          (begin
            (toggle-schild!)
            ((teken-adt 'verwijder-power-up-image!))
            ;(display power-up)
            ((power-up 'toggle-actief!))
            (set! power-up-duur 0)
            (set! power-up-trigger #f))))
    

    ; Bepalen welke procedure moet aangeroepen worden om zo de juiste power-up te activeren
    (define (roep-power-up-op!)
      (let ((type (power-up 'type)))
        (cond ((= type 1) (geef-extra-leven!))
              ((= type 2) (zet-vloot-terug!))
              ((= type 3) (toggle-schild!)))))

    ; Checken wanneer tijdsgebonden power-ups moeten worden uitgezet
    (define (check-power-up-duur)
      (if (and (power-up 'tijdsgebonden?)
               (>= power-up-duur power-up-looptijd))
          (let ((type (power-up 'type)))
            (toggle-schild!))))
    

    ;; ----------> Raket-geraakt? <----------

    ; Checken wanneer de power-up de raket raakt
    (define (check-power-up-geraakt)
      (if (and power-up
               (not power-up-trigger)
               (not (power-up 'actief?)))
          (let* ((raket-pos (raket 'positie))
                 (gelijk? ((raket-pos 'gelijk?) (power-up 'positie))))
            (if gelijk?
                (begin (set! power-up-trigger #t)
                       ((teken-adt 'verwijder-power-up!) power-up)
                       ((teken-adt 'teken-power-up-image)))))))

    ;; ----------> Activatieprocedures <----------

    ; type 1
    ; Voor elke power-up een procedure die hem gaat activeren
    (define (geef-extra-leven!)
      ((raket 'voeg-leven-toe!))
      ((teken-adt 'teken-levens) raket))

    ; type 2
    (define (zet-vloot-terug!)
      ((alienvloot 'zet-vloot-terug!)))

    ; type 3
    (define (toggle-schild!)
      ((raket 'toggle-schild!))
      ((teken-adt 'toggle-raket-schild!) raket))
    

    ;; --------------- SCORE - OPERATIES ---------------


    ; vergelijkt huidige met hoogste score en past indien nodig aan
    ; vergelijk-met-hoogste! / -> /
    (define (vergelijk-met-hoogste!)
      (let ((huidige-score (score 'huidige-score)))
        (if ((score 'meer-dan-hoogste?))
            (begin ((score 'verander-hoogste!) huidige-score)
                   ((teken-adt 'teken-hoogste-score) score)))))


    ; score wordt bepaald op basis van de alien die werd neergeschoten
    ; bepaal-score : Alien -> /
    (define (bepaal-score! alien)
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



    ;; --------------- GAME-OVER / NIEUW LEVEL ---------------


    ; Als vloot bij de raket is dan : game-over? -> true
    ; of als alle aliens zijn vernietigd
    ; check-vloot! : / -> /
    (define (check-vloot!)
      (let ((onderkant? (alienvloot 'onderkant-geraakt?))
            (vernietigd? (alienvloot 'vloot-vernietigd?)))
        (cond (onderkant?
               ((alienvloot 'reset-onderkant-geraakt!))
               (set! game-over-tijd 0)
               (set! game-over? #t))
              (vernietigd?
               ((alienvloot 'reset-vloot-vernietigd!))
               (set! game-over-tijd 0)
               (set! game-over? #t)
               (set! volgend-level? #t)))))
              
      
    ;; maak-nieuw-spel! : / -> /
    (define (maak-nieuw-spel!)
      (if (and game-over?
               (> game-over-tijd game-over-delay))
          (begin (set! game-over? #f)

                 ; alle elementen resetten (vloot, levens, raket en score)

                 ; positie van raket op startpositie
                 ((raket 'positie!) raket-start-positie)
                      
                 (if volgend-level?
                          
                     (begin ((teken-adt 'verwijder-vloot!) alienvloot)
                            ((alienvloot 'vul-vloot!) 'willekeurig)
                            (set! volgend-level? #f)))
                          
                 ; levens van raket terug op 5 zetten
                 ((raket 'reset-levens!))
                 ((teken-adt 'teken-levens) raket)

                 ; vergelijken met record + huidige resetten
                 (vergelijk-met-hoogste!)
                 ((score 'reset-score!))
                 ((teken-adt 'teken-huidige-score) score))))


    ;; --------------- CALLBACKS ---------------
    

    ;; update! : number -> /
    (define (update! tijdsverschil)
      (set! game-over-tijd (+ game-over-tijd tijdsverschil))
      (set! vloot-tijd (+ vloot-tijd tijdsverschil))
      (set! kogel-tijd (+ kogel-tijd tijdsverschil))
      (set! power-up-tijd (+ power-up-tijd tijdsverschil))
      (set! alien-schiettijd (+ alien-schiettijd tijdsverschil))
      (set! power-up-duur (+ power-up-duur tijdsverschil))
      (maak-nieuw-spel!)
      ; Zolang niet game-over? blijf dit doen...
      (if (not game-over?)
          (begin (beweeg-vloot!)
                 (beweeg-kogels!)
                 (beweeg-power-up!)
                 (schiet-alienkogel!)
                 (check-kogels-geraakt)
                 (check-power-up-geraakt)
                 (check-vloot!)))
      ; Als er een power-up is + actief is...
      (if (and power-up (power-up 'actief?))
          (check-power-up-duur)))
    
    
    ;; toets! : any -> /
    (define (toets! toets)
      (if (not game-over?)
          (begin (beweeg-raket! toets)
                 (schiet-raketkogel! toets)
                 (activeer-power-up! toets))))
    
    
    ; Dispatch
    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'toets!) toets!)
            ((eq? msg 'raket) raket)
            ((eq? msg 'kogels) kogels)
            ((eq? msg 'power-up) power-up)
            ((eq? msg 'power-up-trigger) power-up-trigger)
            ((eq? msg 'alienvloot) alienvloot)
            (else (display "ongeldige boodschap"))))

    dispatch-level))
            

    
