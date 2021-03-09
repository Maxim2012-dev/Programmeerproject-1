;;            ----------------
;; ----------  Alienvloot ADT  ----------
;;            ----------------

(load "matrix.rkt")
(load "alienschip-adt.rkt")
(load "positie-adt.rkt")

(define (maak-alienvloot)
  (let* ((schepen (maak-matrix))
         (richting 'rechts)
         (size (vector-length schepen)))


    (define (afstand-tussen-rijen idx) (* 1 idx))
    (define (afstand-tussen-kolommen idx) (* 2 idx))

    ; Elke plaats in de vector opvullen met een alienschip object
    (define (vul-vloot)
      (let outer-loop
        ((outer-idx 0))
        (if (< outer-idx size)
            (let inner-loop
              ((inner-idx 0)
               (inner-vector (vector-ref schepen outer-idx)))
              ; inner-idx + 1 (vermijden dat x op nul start)
              (define nieuw_alienschip (maak-alienschip (maak-positie
                                                         (afstand-tussen-kolommen (+ inner-idx 1))
                                                         (afstand-tussen-rijen outer-idx))))
              ; kleur toewijzen aan het nieuw alienschip
              (cond ((= outer-idx 0) ((nieuw_alienschip 'kleur!) 'paars))
                    ((= outer-idx 1) ((nieuw_alienschip 'kleur!) 'blauw))
                    ((= outer-idx 2) ((nieuw_alienschip 'kleur!) 'geel))
                    (else ((nieuw_alienschip 'kleur!) 'blauw)))
              (vector-set! inner-vector inner-idx nieuw_alienschip)
              (if (< (+ inner-idx 1) aantal-aliens-per-rij)
                  (inner-loop (+ inner-idx 1) inner-vector)
                  (outer-loop (+ outer-idx 1)))))))
    
    
    (vul-vloot)
    

    ; Een for-each om op elk alienschip een functie los te laten
    (define (voor-alle-schepen fun)
      (let outer-loop
        ((outer-idx 0))
        (if (< outer-idx size)
            (let inner-loop
              ((inner-idx 0)
               (inner-vector (vector-ref schepen outer-idx)))
              (fun (vector-ref inner-vector inner-idx))
              (if (< (+ inner-idx 1) aantal-aliens-per-rij)
                  (inner-loop (+ inner-idx 1) inner-vector)
                  (outer-loop (+ outer-idx 1)))))))
    


    ; verwijder-schip! : alienschip-adt -> /
    ; gaat op zoek naar het alienschip dat 'verwijderd' moet worden en zet zijn y-positie dan buiten
    ; het speelveld en verandert de status naar inactief. (je kan immers niet echt een element verwijderen uit een vector)
    (define (verwijder-schip! alienschip-adt)
      (voor-alle-schepen (lambda (alien) (if (eq? alien alienschip-adt)
                                             (begin
                                               (((alien 'positie) 'y!) y-buiten-speelveld)
                                               ((alien 'zet-inactief!)))))))
    

    ; switch! : / -> /
    ; Het alterneren van de richting van het alienvloot
    (define (switch!)
      (if (eq? richting 'rechts)
          (set! richting 'links)
          (set! richting 'rechts)))

    
    ; beweeg! : / -> /
    (define (beweeg!)
      ; rand checken
      (let ((test-list '()))
        (voor-alle-schepen (lambda (schip)
                             (set! test-list (cons ((schip 'rand-geraakt?)) test-list))))
        (if (not (member #t test-list))
            (voor-alle-schepen roep-beweeg-op)
            (begin (switch!)
                   (voor-alle-schepen (lambda (schip)
                                        ((schip 'beweeg) 'omlaag)))
                   (voor-alle-schepen roep-beweeg-op)))))


    ; roep-beweeg-op : alienschip-adt -> /
    (define (roep-beweeg-op schip-adt)
      ((schip-adt 'beweeg) richting))


    ;; dispatch-functie
    (define (dispatch-alienvloot msg)
      (cond ((eq? msg 'beweeg) beweeg!)
            ((eq? msg 'schepen) schepen)
            ((eq? msg 'verwijder-schip!) verwijder-schip!)
            ((eq? msg 'voor-alle-schepen) voor-alle-schepen)
            (else "verkeerde boodschap - alienvloot")))
    dispatch-alienvloot))
                  
              
              
        
