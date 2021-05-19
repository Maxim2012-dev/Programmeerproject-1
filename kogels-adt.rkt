(define (maak-kogels-adt)
  (let ((kogels-lijst (cons 'kogels '())))

    ; headed-list maken

    ; voeg-kogel-toe! : Kogel -> /
    (define (voeg-kogel-toe! kogel)
      (set-cdr! kogels-lijst (cons kogel (cdr kogels-lijst))))


    ; maak-lijst-leeg! : / -> /
    (define (maak-lijst-leeg!)
      (set-cdr! kogels-lijst '()))


    ; voor-alle-kogels : <procedure> -> /
    (define (voor-alle-kogels fun)
      (define (iter list)
        (if (not (null? list))
            (begin (fun (car list))
                   (iter (cdr list)))))
      (iter (cdr kogels-lijst)))

    ; verwijder-kogel! : Kogel -> /
    (define (verwijder-kogel! kogel)
      (define (iter lijst)
        (if (not (null? (cdr lijst)))
            (cond ((eq? kogel (cadr lijst))
                   (set-cdr! lijst (cddr lijst)))
                  (else (iter (cdr lijst))))))
      (iter kogels-lijst))


    ;; dispatch functie
    (define (dispatch-kogels msg)
      (cond ((eq? msg 'kogels-lijst) kogels-lijst)
            ((eq? msg 'maak-lijst-leeg!) maak-lijst-leeg!)
            ((eq? msg 'voeg-kogel-toe!) voeg-kogel-toe!)
            ((eq? msg 'voor-alle-kogels) voor-alle-kogels)
            ((eq? msg 'verwijder-kogel!) verwijder-kogel!)
            (else (display "ongeldige boodschap - kogels"))))

    dispatch-kogels))