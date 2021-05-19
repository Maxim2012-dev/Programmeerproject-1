;;           -----------
;;----------  Teken ADT  ----------
;;           -----------

(#%require "Graphics.rkt")

; constructor voor teken-adt
; (number number -> Teken)
(define (maak-teken-adt pixels-horizontaal pixels-verticaal)
  (let ((scherm (make-window pixels-horizontaal pixels-verticaal "Space Invaders")))

    ((scherm 'set-background!) "black")


    ;; --------------- LAGEN EN TILES ---------------

    ; Raketlaag en tile 
    (define raket-laag (scherm 'make-layer))
    (define raket-tile
      (make-bitmap-tile "afbeeldingen/raket.png"))
    ((raket-laag 'add-drawable) raket-tile)

    ; Alienlaag 
    (define alien-laag (scherm 'make-layer))
    (define alien-tiles '())

    ; Kogel / Power-Up-laag en tiles-lijst
    (define kogel-laag (scherm 'make-layer))
    (define kogel-tiles '())
    
    (define power-up-tile #f)
    (define power-up-image-tile (make-bitmap-tile "afbeeldingen/power-up-image.png"))

    ((power-up-image-tile 'set-x!) power-up-img-x)
    ((power-up-image-tile 'set-y!) power-up-img-y)

    ; Score / Levenslaag en tiles
    (define score-levenslaag (scherm 'make-layer))

    ; ---------> Score <---------
    (define score-label-tile (make-tile 100 40))
    (define punten-tile (make-tile 60 40))
    (define record-label-tile (make-tile 100 40))
    (define record-punten-tile (make-tile 60 40))

    ((score-label-tile 'draw-text) "score : " 20 5 5 "white")
    ((punten-tile 'draw-text) "000" 20 5 5 "white")
    ((record-label-tile 'draw-text) "record : " 20 5 5 "white")
    ((record-punten-tile 'draw-text) "000" 20 5 5 "white")

    ((score-levenslaag 'add-drawable) score-label-tile)
    ((score-levenslaag 'add-drawable) punten-tile)
    ((score-levenslaag 'add-drawable) record-label-tile)
    ((score-levenslaag 'add-drawable) record-punten-tile)

    ((score-label-tile 'set-x!) score-label-tekst-x)
    ((score-label-tile 'set-y!) score-label-tekst-y)
    ((punten-tile 'set-x!) punten-tekst-x)
    ((punten-tile 'set-y!) punten-tekst-y)

    ((record-label-tile 'set-x!) record-label-tekst-x)
    ((record-label-tile 'set-y!) record-label-tekst-y)
    ((record-punten-tile 'set-x!) record-punten-tekst-x)
    ((record-punten-tile 'set-y!) record-punten-tekst-y)

    ; ---------> Levens <---------
    (define levens-tile (make-tile 32 32))
    (define raket-image-tile (make-bitmap-tile "afbeeldingen/raket_image.png"))
    ((levens-tile 'draw-text) "5" 20 0 0 "white")

    ((score-levenslaag 'add-drawable) levens-tile)
    ((score-levenslaag 'add-drawable) raket-image-tile)
    
    ((levens-tile 'set-x!) levens-x)
    ((levens-tile 'set-y!) levens-y)
    ((raket-image-tile 'set-x!) raket-image-x)
    ((raket-image-tile 'set-y!) raket-image-y)
    
    ;; --------------- TILES GENEREREN ---------------


    ;; ----------> Aliens <----------
    

    ;; voeg-alienschip-toe! : Alienschip -> tile
    ; Maakt een bitmap-tile aan voor een bepaald alienschipobject
    (define (voeg-alienschip-toe! alienschip-adt)
      (let* ((kleur (alienschip-adt 'kleur))
             (nieuwe-tile (cond ((eq? kleur 'geel) (make-bitmap-tile "afbeeldingen/alien_geel.png"))
                                ((eq? kleur 'blauw) (make-bitmap-tile "afbeeldingen/alien_blauw.png"))
                                ((eq? kleur 'paars) (make-bitmap-tile "afbeeldingen/alien_paars.png")))))
        (set! alien-tiles (cons (cons alienschip-adt nieuwe-tile) alien-tiles))
        ((alien-laag 'add-drawable) nieuwe-tile)
        nieuwe-tile))
    

    ;; neem-alienschip : Alienschip -> tile
    ; De procedure haalt op basis van een gegeven alienschipobject de overeenkomstige
    ; tile uit alien-tiles.
    (define (neem-alienschip alienschip-adt)
      (let ((resultaat (assoc alienschip-adt alien-tiles)))
        (if resultaat
            (cdr resultaat)
            (voeg-alienschip-toe! alienschip-adt))))
    

    ;; ----------> Kogels <----------


    ;; voeg-kogel-toe! : Kogel -> tile
    ; maakt een nieuwe tile aan en voegt deze toe aan de laag en geeft deze dan terug
    (define (voeg-kogel-toe! kogel)
      (let ((nieuwe-tile (if (kogel 'torpedo?)
                             (make-bitmap-tile "afbeeldingen/torpedo.png")
                             (make-bitmap-tile "afbeeldingen/kogel.png" "afbeeldingen/kogel-mask.png"))))
        (set! kogel-tiles (cons (cons kogel nieuwe-tile) kogel-tiles))
        ((kogel-laag 'add-drawable) nieuwe-tile)
        nieuwe-tile))


    ;; neem-kogel : Kogel -> tile
    (define (neem-kogel kogel-adt)
      (let ((resultaat (assoc kogel-adt kogel-tiles)))
        (if resultaat
            (cdr resultaat)
            (voeg-kogel-toe! kogel-adt))))

    ;; ----------> Power-Ups <----------

    ;; neem-power-up! : / -> /
    (define (neem-power-up!)
      (if (not power-up-tile)
          (begin (set! power-up-tile (make-bitmap-tile "afbeeldingen/power-up.png"))
                 ((kogel-laag 'add-drawable) power-up-tile))))
    


    ;; --------------- TEKEN FUNCTIES ---------------

    ;; Generische teken functie
    ; Deze functie vereenvoudigd het tekenproces van de objecten in het spel
    ; aangezien de functie eender welk object en overeenkomstige tile kan ontvangen en
    ; deze dan op de juiste positie op het scherm tekent.
    ;; teken-object! : any tile -> /  (hogere orde procedure)
    (define (teken-object! obj tile)
      (let* ((obj-x ((obj 'positie) 'x))
             (obj-y ((obj 'positie) 'y))
             (scherm-x (* cel-breedte-px obj-x))
             (scherm-y (* cel-hoogte-px obj-y)))
        ((tile 'set-x!) scherm-x)
        ((tile 'set-y!) scherm-y)))
    

    ;; ---------------> Tekenen <---------------
    
    ;; Raket
    ;; teken-raket! : Raket -> /
    (define (teken-raket! raket)
      (teken-object! raket raket-tile))

    ;; Raket
    ;; toggle-raket-schild! : Raket -> /
    (define (toggle-raket-schild! raket)
      (if (raket 'schild?)
          (begin ((raket-laag 'remove-drawable) raket-tile)
                 (set! raket-tile (make-bitmap-tile "afbeeldingen/raket-schild.png"))
                 ((raket-laag 'add-drawable) raket-tile))
          (begin
            ((raket-laag 'remove-drawable) raket-tile)
            (set! raket-tile (make-bitmap-tile "afbeeldingen/raket.png"))
            ((raket-laag 'add-drawable) raket-tile))))

    ;; Spel
    ;; teken-spel! : Spel -> /
    (define (teken-spel! spel-adt)
      (teken-level! (spel-adt 'level)))

    ;; Level
    ;; teken-level! : Level -> /
    (define (teken-level! level-adt)
      (teken-raket! (level-adt 'raket))
      (teken-vloot! (level-adt 'alienvloot))
      (teken-kogels! (level-adt 'kogels))
      (teken-power-up! (level-adt 'power-up)))

    ;; Alienschip
    ;; teken-alienschip! : Alienschip -> /
    (define (teken-alienschip! alienschip-adt)
      (let ((tile (neem-alienschip alienschip-adt)))
        (teken-object! alienschip-adt tile)))

    ;; Alienvloot
    ;; teken-vloot! : Vloot -> /
    (define (teken-vloot! alienvloot-adt)
      ((alienvloot-adt 'voor-alle-schepen) teken-alienschip!))

    ;; Kogel
    ;; teken-kogel! : Kogel -> /
    (define (teken-kogel! kogel)
      (let ((tile (neem-kogel kogel)))
        (teken-object! kogel tile)))

    ;; Kogel
    ;; teken-kogels! : list -> /
    (define (teken-kogels! kogels-lijst)
      ((kogels-lijst 'voor-alle-kogels) teken-kogel!))

    ;; teken-levens : Raket -> /
    (define (teken-levens raket)
      (let* ((aantal-levens (raket 'levens))
             (levens-tekst (number->string aantal-levens)))
        (levens-tile 'clear)
        ((levens-tile 'draw-text) levens-tekst 20 0 0 "white")))

    ;; Power-Up
    ;; teken-power-up! : Power-Up -> /
    (define (teken-power-up! power-up)
      (if (and power-up
               (not (power-up 'opgenomen?))
               (not (power-up 'actief?)))
          (begin (neem-power-up!)
                 (teken-object! power-up power-up-tile))))

    ;; Power-Up
    ;; teken-power-up-image : / -> /
    (define (teken-power-up-image)
      ((kogel-laag 'add-drawable) power-up-image-tile))

    ;; huidige score
    ;; teken-huidige-score! : Score -> /
    (define (teken-huidige-score score)
      (let* ((huidige-score (score 'huidige-score))
             (score-tekst (number->string huidige-score)))
        (punten-tile 'clear)
        (if (< huidige-score 100)
            ((punten-tile 'draw-text) (string-append "0" score-tekst) 20 5 5 "white")
            ((punten-tile 'draw-text) score-tekst 20 5 5 "white"))))

    ;; hoogste score
    ;; teken-hoogste-score! : 
    (define (teken-hoogste-score score)
      (let* ((hoogste-score (score 'hoogste-score))
             (score-tekst (number->string hoogste-score)))
        (record-punten-tile 'clear)
        (if (< hoogste-score 100)
            ((record-punten-tile 'draw-text) (string-append "0" score-tekst) 20 5 5 "white")
            ((record-punten-tile 'draw-text) score-tekst 20 5 5 "white"))))

    ;; ---------------> Verwijderen <---------------

    
    ;; verwijder-kogel! : Kogel -> /
    (define (verwijder-kogel! kogel)
      (let ((tile (neem-kogel kogel)))
        ((kogel-laag 'remove-drawable) tile)))

    ;; verwijder-vloot! : Alienvloot -> /
    (define (verwijder-vloot! alienvloot)
      ((alienvloot 'voor-alle-schepen) verwijder-alien!))

    ;; verwijder-alien! : Alien -> /
    (define (verwijder-alien! alien)
      (let ((tile (neem-alienschip alien)))
        ((alien-laag 'remove-drawable) tile)))

    ;; verwijder-power-up! : Power-Up -> /
    (define (verwijder-power-up!)
      (display power-up-tile) (newline)
      ((kogel-laag 'remove-drawable) power-up-tile)
      (set! power-up-tile #f))

    ;; verwijder-power-up-image! : / -> /
    (define (verwijder-power-up-image!)
      ((kogel-laag 'remove-drawable) power-up-image-tile))
    

    ;; --------------- CALLBACKS INSTELLEN ---------------

    ;; set-spel-lus-functie! : (number -> /) -> /
    (define (set-spel-lus-functie! func)
      ((scherm 'set-update-callback!) func))

    ;; set-toets-functie! : (symbol, any -> /) -> /
    (define (set-toets-functie! func)
      ((scherm 'set-key-callback!) func))


    ;; Dispatch
    (define (dispatch-teken msg)
      (cond ((eq? msg 'set-toets-functie!) set-toets-functie!)
            ((eq? msg 'set-spel-lus-functie!) set-spel-lus-functie!)
            ((eq? msg 'teken-spel!) teken-spel!)
            ((eq? msg 'teken-huidige-score) teken-huidige-score)
            ((eq? msg 'teken-hoogste-score) teken-hoogste-score)
            ((eq? msg 'teken-levens) teken-levens)
            ((eq? msg 'teken-power-up-image) teken-power-up-image)
            ((eq? msg 'toggle-raket-schild!) toggle-raket-schild!)
            ((eq? msg 'verwijder-power-up-image!) verwijder-power-up-image!)
            ((eq? msg 'verwijder-kogel!) verwijder-kogel!)
            ((eq? msg 'verwijder-alien!) verwijder-alien!)
            ((eq? msg 'verwijder-vloot!) verwijder-vloot!)
            ((eq? msg 'verwijder-power-up!) verwijder-power-up!)
            (else (display "geen geldige boodschap"))))

    dispatch-teken))




