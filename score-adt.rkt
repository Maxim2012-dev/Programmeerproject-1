;;            -----------
;; ----------  Score ADT  ----------
;;            -----------

(define (maak-score)
  (let ((huidige-score 0)
        (hoogste-score 0))

    
    ;; Het aantal punten kan verschillen per alien dat je neerschiet
    ;; verhoog-score! : number -> /
    (define (verhoog-score! punten)
      (set! huidige-score (+ huidige-score punten)))


    ;; Als de raket dood is, moet enkel deze gereset worden
    ;; reset-score! : / -> /
    (define (reset-score!)
      (set! huidige-score 0))


    ;; verander-hoogste! : number -> /
    (define (verander-hoogste! score)
      (set! hoogste-score score))


    ;; Predicaat om te checken of huidige score groter is dan de hoogste
    ;; meer-dan-hoogste? : / -> boolean
    (define (meer-dan-hoogste?)
      (> huidige-score hoogste-score))


    ;; dispatch procedure
    (define (dispatch-score msg)
      (cond ((eq? msg 'huidige-score) huidige-score)
            ((eq? msg 'verhoog-score!) verhoog-score!)
            ((eq? msg 'reset-score!) reset-score!)
            ((eq? msg 'verander-hoogste!) verander-hoogste!)
            ((eq? msg 'meer-dan-hoogste?) meer-dan-hoogste?)
            (else (display "ongeldige boodschap - score"))))
    dispatch-score))
    