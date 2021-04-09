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

    ; Kogellaag en tiles-lijst
    (define kogel-laag (scherm 'make-layer))
    (define kogel-tiles '())

    ; Scorelaag en tiles
    (define score-laag (scherm 'make-layer))
    (define score-label-tile (make-tile 100 40))
    (define punten-tile (make-tile 60 40))
    (define record-label-tile (make-tile 100 40))
    (define record-punten-tile (make-tile 60 40))

;    ((score-label-tile 'draw-rectangle) 0 0 100 40 "red")
;    ((punten-tile 'draw-rectangle) 0 0 60 40 "red")
;    ((record-label-tile 'draw-rectangle) 0 0 100 40 "red")
;    ((record-punten-tile 'draw-rectangle) 0 0 60 40 "red")

    ((score-label-tile 'draw-text) "score : " 20 5 5 "white")
    ((punten-tile 'draw-text) "000" 20 5 5 "white")
    ((record-label-tile 'draw-text) "record : " 20 5 5 "white")
    ((record-punten-tile 'draw-text) "000" 20 5 5 "white")

    ((score-laag 'add-drawable) score-label-tile)
    ((score-laag 'add-drawable) punten-tile)
    ((score-laag 'add-drawable) record-label-tile)
    ((score-laag 'add-drawable) record-punten-tile)

    ((score-label-tile 'set-x!) score-label-tekst-x)
    ((score-label-tile 'set-y!) score-label-tekst-y)
    ((punten-tile 'set-x!) punten-tekst-x)
    ((punten-tile 'set-y!) punten-tekst-y)

    ((record-label-tile 'set-x!) record-label-tekst-x)
    ((record-label-tile 'set-y!) record-label-tekst-y)
    ((record-punten-tile 'set-x!) record-punten-tekst-x)
    ((record-punten-tile 'set-y!) record-punten-tekst-y)

    ;; --------------- TILES GENEREREN ---------------
    

    ;; voeg-alienschip-toe! : alienschip -> tile
    ; Maakt een bitmap-tile aan voor een bepaald alienschipobject
    (define (voeg-alienschip-toe! alienschip-adt)
      (let* ((kleur (alienschip-adt 'kleur))
            (nieuwe-tile (cond ((eq? kleur 'geel) (make-bitmap-tile "afbeeldingen/alien_geel.png"))
                               ((eq? kleur 'blauw) (make-bitmap-tile "afbeeldingen/alien_blauw.png"))
                               ((eq? kleur 'paars) (make-bitmap-tile "afbeeldingen/alien_paars.png")))))
        (set! alien-tiles (cons (cons alienschip-adt nieuwe-tile) alien-tiles))
        ((alien-laag 'add-drawable) nieuwe-tile)
        nieuwe-tile))
    

    ;; neem-alienschip : alienschip -> tile
    ; De procedure haalt op basis van een gegeven alienschipobject de overeenkomstige
    ; tile uit alien-tiles.
    (define (neem-alienschip alienschip-adt)
      (let ((resultaat (assoc alienschip-adt alien-tiles)))
        (if resultaat
            (cdr resultaat)
            (voeg-alienschip-toe! alienschip-adt))))


    ;; voeg-kogel-toe! : / -> tile
    ; maakt een nieuwe tile aan en voegt deze toe aan de laag en geeft deze dan terug
    (define (voeg-kogel-toe! kogel-adt)
      (let ((nieuwe-tile
             (make-bitmap-tile "afbeeldingen/kogel.png" "afbeeldingen/kogel-mask.png")))
        (set! kogel-tiles (cons (cons kogel-adt nieuwe-tile) kogel-tiles))
        ((kogel-laag 'add-drawable) nieuwe-tile)
        nieuwe-tile))


    ;; neem-kogel : kogel -> tile
    (define (neem-kogel kogel-adt)
      (let ((resultaat (assoc kogel-adt kogel-tiles)))
        (if resultaat
            (cdr resultaat)
            (voeg-kogel-toe! kogel-adt))))
    


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
    (define (teken-raket! raket-adt)
      (if raket-adt
          (teken-object! raket-adt raket-tile)))

    ;; Spel
    ;; teken-spel! : Spel -> /
    (define (teken-spel! spel-adt)
      (teken-level! (spel-adt 'level)))

    ;; Level
    ;; teken-level! : Level -> /
    (define (teken-level! level-adt)
      (teken-raket! (level-adt 'raket))
      (teken-vloot! (level-adt 'alienvloot))
      (teken-kogels! (level-adt 'kogels)))

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
    (define (teken-kogel! kogel-adt)
      (let ((tile (neem-kogel kogel-adt)))
          (teken-object! kogel-adt tile)))

    ;; teken-kogels! : list -> /
    (define (teken-kogels! kogels-lijst)
      (let ((lijst (kogels-lijst 'kogels-lijst)))
        ((kogels-lijst 'voor-alle-kogels) teken-kogel!)))

    ;; Score
    ;; teken-score! : 
    (define (teken-score score)
      (let* ((huidige-score (score 'huidige-score))
             (score-tekst (number->string huidige-score)))
        (punten-tile 'clear)
        (if (< huidige-score 100)
            ((punten-tile 'draw-text) (string-append "0" score-tekst) 20 5 5 "white")
            ((punten-tile 'draw-text) score-tekst 20 5 5 "white"))))

    ;; ---------------> Verwijderen <---------------

    
    ;; verwijder-kogel! : Kogel -> /
    (define (verwijder-kogel! kogel-adt)
      (let ((tile (neem-kogel kogel-adt)))
        ((kogel-laag 'remove-drawable) tile)))
    

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
            ((eq? msg 'teken-score) teken-score)
            ((eq? msg 'verwijder-kogel!) verwijder-kogel!)
            (else (display "geen geldige boodschap"))))

    dispatch-teken))




