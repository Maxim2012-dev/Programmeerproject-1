;;            ----------------
;; ----------  Alienvloot ADT  ----------
;;            ----------------

(load "matrix.rkt")
(load "alienschip-adt.rkt")
(load "positie-adt.rkt")
(#%require srfi/27)

(define (maak-alienvloot)
  (let* ((schepen (maak-matrix))
         (richting 'rechts)
         (size (vector-length schepen))
         (onderkant-geraakt? #f)
         (vloot-vernietigd? #f))


    (define (afstand-tussen-rijen idx) (+ 3 idx))
    (define (afstand-tussen-kolommen idx) (* 2 idx))

    ; Elke plaats in de vector opvullen met een alienschip object
    (define (vul-vloot! type)
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
              ; in het geval van een willekeurig vloot
              (if (eq? type 'willekeurig)
                  (bepaal-willekeurig! nieuw_alienschip))
              ; kleur + levens toewijzen aan het nieuw alienschip
              (cond ((= outer-idx 0) ((nieuw_alienschip 'kleur!) 'paars)
                                     ((nieuw_alienschip 'levens!) 3))
                    ((= outer-idx 1) ((nieuw_alienschip 'kleur!) 'blauw)
                                     ((nieuw_alienschip 'levens!) 1))
                    ((= outer-idx 2) ((nieuw_alienschip 'kleur!) 'geel)
                                     ((nieuw_alienschip 'levens!) 2))
                    (else ((nieuw_alienschip 'kleur!) 'blauw)
                          ((nieuw_alienschip 'levens!) 1)))
              (vector-set! inner-vector inner-idx nieuw_alienschip)
              (if (< (+ inner-idx 1) aantal-aliens-per-rij)
                  (inner-loop (+ inner-idx 1) inner-vector)
                  (outer-loop (+ outer-idx 1)))))))
    

    ; Er wordt een random-waarde berekend en slechts als deze 1 is wordt
    ; de huidige alien op actief gezet + buiten speelveld gezet
    ; bepaal-willekeurig : Alienschip -> /
    (define (bepaal-willekeurig! alien)
      (define random (random-integer 2))
      (if (= random 1)
          (begin
            ((alien 'zet-inactief!))
            (((alien 'positie) 'y!) y-buiten-speelveld))))
    
    
    (vul-vloot! 'normaal)
    

    ; Een for-each om op elk alienschip een functie los te laten
    ; voor-alle-schepen : <procedure> -> /
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
      (let ((edge-list '())
            (status-list '()))
        ; checken op zijranden en onderkant + status aliens bijhouden
        (define (check-pos alien)
          (cond (((alien 'rand-geraakt?))
                 (set! edge-list (cons 'rand edge-list)))
                (((alien 'onderkant?))
                 (set! edge-list (cons 'onderkant edge-list)))
                (else (set! edge-list (cons #t edge-list))))
          (set! status-list (cons (alien 'status) status-list)))
        ; toepassen op alle aliens
        (voor-alle-schepen check-pos)
        ; als geen enkele alien meer op actief staat, dan is het vloot vernietigd
        (if (not (member 'actief status-list))
            (set! vloot-vernietigd? #t))
        ; bepalen wat er moet gebeuren op basis van edge-list
        (cond
          ((member 'onderkant edge-list)

           (set! onderkant-geraakt? #t))
          ((member 'rand edge-list)
           (switch!)
           (voor-alle-schepen (lambda (schip)
                                ((schip 'beweeg!) 'omlaag)))
           (voor-alle-schepen roep-beweeg-op))
          (else (voor-alle-schepen roep-beweeg-op)))))


    ; roep-beweeg-op : alienschip-adt -> /
    (define (roep-beweeg-op schip-adt)
      ((schip-adt 'beweeg!) richting))

    ; reset-onderkant-geraakt! : / -> /
    (define (reset-onderkant-geraakt!)
      (set! onderkant-geraakt? #f))

    ; reset-vloot-vernietigd! : / -> /
    (define (reset-vloot-vernietigd!)
      (set! vloot-vernietigd? #f))


    ;; dispatch-functie
    (define (dispatch-alienvloot msg)
      (cond ((eq? msg 'beweeg) beweeg!)
            ((eq? msg 'schepen) schepen)
            ((eq? msg 'verwijder-schip!) verwijder-schip!)
            ((eq? msg 'voor-alle-schepen) voor-alle-schepen)
            ((eq? msg 'vul-vloot!) vul-vloot!)
            ((eq? msg 'onderkant-geraakt?) onderkant-geraakt?)
            ((eq? msg 'vloot-vernietigd?) vloot-vernietigd?)
            ((eq? msg 'reset-onderkant-geraakt!) reset-onderkant-geraakt!)
            ((eq? msg 'reset-vloot-vernietigd!) reset-vloot-vernietigd!)
            (else "verkeerde boodschap - alienvloot")))
    dispatch-alienvloot))
                  
              
              
        
