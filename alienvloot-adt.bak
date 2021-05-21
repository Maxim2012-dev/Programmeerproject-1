;;            ----------------
;; ----------  Alienvloot ADT  ----------
;;            ----------------

(load "alienschip-adt.rkt")
(load "positie-adt.rkt")
(#%require srfi/27)

(define (maak-alienvloot)
  (let ((schepen #f)
        (richting 'rechts)
        (size vloot-size)
        (aantal-vernietigde-schepen 0)
        (onderkant-geraakt? #f)
        (vloot-vernietigd? #f)
        (vlootsnelheid 1000))
    

    ;; Aanmaken van een matrix-datastructuur
    ;; zodat we daarin onze alienschepen kunnen plaatsen (vector van vectoren)
    (define (maak-matrix)
      (let ((vector (make-vector aantal-rijen-aliens)))
        (let loop
          ((huidige-idx 0))
          (if (< huidige-idx aantal-rijen-aliens)
              (begin
                (vector-set! vector huidige-idx (make-vector aantal-aliens-per-rij))
                (loop (+ huidige-idx 1)))))
        vector))

    (set! schepen (maak-matrix))
    

    (define (afstand-tussen-aliens idx) (* 2 idx))

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
                                                         (afstand-tussen-aliens (+ inner-idx 1))
                                                         (afstand-tussen-aliens (+ outer-idx 2)))))
              ; in het geval van een willekeurig vloot
              (if (eq? type 'willekeurig)
                  (bepaal-willekeurig! nieuw_alienschip))
              ; kleur + levens toewijzen aan het nieuw alienschip
              (cond ((= outer-idx 0) ((nieuw_alienschip 'kleur!) 'groen)
                                     ((nieuw_alienschip 'levens!) 3))
                    ((= outer-idx 1) ((nieuw_alienschip 'kleur!) 'geel)
                                     ((nieuw_alienschip 'levens!) 1))
                    ((= outer-idx 2) ((nieuw_alienschip 'kleur!) 'paars)
                                     ((nieuw_alienschip 'levens!) 2))
                    (else ((nieuw_alienschip 'kleur!) 'geel)
                          ((nieuw_alienschip 'levens!) 1)))
              (vector-set! inner-vector inner-idx nieuw_alienschip)
              (if (< (+ inner-idx 1) aantal-aliens-per-rij)
                  (inner-loop (+ inner-idx 1) inner-vector)
                  (outer-loop (+ outer-idx 1)))))))
    

    ; Er wordt een random-waarde berekend en slechts als deze 1 is wordt
    ; de huidige alien op inactief gezet + buiten speelveld gezet
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


    ; zet-vloot-terug! : / -> /
    (define (zet-vloot-terug!)
      (let outer-loop
        ((outer-idx 0))
        (if (< outer-idx size)
            (let inner-loop
              ((inner-idx 0)
               (inner-vector (vector-ref schepen outer-idx)))
              ; omgevingsmodel laat ons niet toe dit in de named-let te zetten
              ; dan zouden we een soort van named-let* nodig moeten hebben...
              (define alien (vector-ref inner-vector inner-idx))
              (if (eq? (alien 'status) 'actief)
                  (begin (((alien 'positie) 'x!) (afstand-tussen-aliens (+ inner-idx 1)))
                         (((alien 'positie) 'y!) (afstand-tussen-aliens (+ outer-idx 2)))))
              (if (< (+ inner-idx 1) aantal-aliens-per-rij)
                  (inner-loop (+ inner-idx 1) inner-vector)
                  (outer-loop (+ outer-idx 1)))))))
        
      

    ; verhoog-vernietigde-schepen! : / -> /
    (define (verhoog-vernietigde-schepen!)
      (set! aantal-vernietigde-schepen
            (+ aantal-vernietigde-schepen 1)))

    ; snelheid-vloot -> constanten.rkt
    ; verhoog-vlootsnelheid! : / -> /
    (define (verhoog-vlootsnelheid!)
      (set! snelheid-vloot (- snelheid-vloot 100)))

    ; roep-beweeg-op : alienschip-adt -> /
    (define (roep-beweeg-op schip-adt)
      ((schip-adt 'beweeg!) richting))

    ; reset-onderkant-geraakt! : / -> /
    (define (reset-onderkant-geraakt!)
      (set! onderkant-geraakt? #f))

    ; reset-vloot-vernietigd! : / -> /
    (define (reset-vloot-vernietigd!)
      (set! vloot-vernietigd? #f))

    ; reset-aantal-vernietigde-schepen! : / -> /
    (define (reset-aantal-vernietigde-schepen!)
      (set! aantal-vernietigde-schepen 0))

    ; reset-vlootsnelheid! : / -> /
    (define (reset-vlootsnelheid!)
      (set! vlootsnelheid snelheid-vloot))


    ;; dispatch-functie
    (define (dispatch-alienvloot msg)
      (cond ((eq? msg 'beweeg) beweeg!)
            ((eq? msg 'schepen) schepen)
            ((eq? msg 'aantal-vernietigde-schepen) aantal-vernietigde-schepen)
            ((eq? msg 'verhoog-vernietigde-schepen!) verhoog-vernietigde-schepen!)
            ((eq? msg 'verwijder-schip!) verwijder-schip!)
            ((eq? msg 'voor-alle-schepen) voor-alle-schepen)
            ((eq? msg 'vul-vloot!) vul-vloot!)
            ((eq? msg 'zet-vloot-terug!) zet-vloot-terug!)
            ((eq? msg 'onderkant-geraakt?) onderkant-geraakt?)
            ((eq? msg 'vlootsnelheid) vlootsnelheid)
            ((eq? msg 'vloot-vernietigd?) vloot-vernietigd?)
            ((eq? msg 'reset-onderkant-geraakt!) reset-onderkant-geraakt!)
            ((eq? msg 'reset-vloot-vernietigd!) reset-vloot-vernietigd!)
            ((eq? msg 'reset-vlootsnelheid!) reset-vlootnsnelheid!)
            ((eq? msg 'reset-aantal-vernietigde-schepen!) reset-aantal-vernietigde-schepen!) 
            (else (display "verkeerde boodschap - alienvloot"))))
    dispatch-alienvloot))
                  
              
              
        
