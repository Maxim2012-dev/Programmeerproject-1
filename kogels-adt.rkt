(define (maak-kogels-adt)
  (let ((kogels-lijst '()))

    (define (voeg-kogel-toe! kogel-adt)
      (set! kogels-lijst (cons kogel-adt kogels-lijst)))


    (define (voor-alle-kogels fun)
      (define (iter list)
        (if (not (null? list))
            (begin (fun (car list))
                   (iter (cdr list)))))
      (iter kogels-lijst))


    (define (verwijder-kogel! kogel-adt)
      (define (iter lijst)
        (if (not (null? lijst))
            (cond ((eq? kogel-adt (car lijst))
                   (set! kogels-lijst (cdr lijst)))
                  ((eq? kogels-adt (cadr lijst))
                   (set-cdr! lijst (cddr lijst)))
                  (else (iter (cdr lijst))))))
      (iter kogels-lijst))


    ;; dispatch functie
    (define (dispatch-kogels msg)
      (cond ((eq? msg 'kogels-lijst) kogels-lijst)
            ((eq? msg 'voeg-kogel-toe!) voeg-kogel-toe!)
            ((eq? msg 'voor-alle-kogels) voor-alle-kogels)
            ((eq? msg 'verwijder-kogel!) verwijder-kogel!)
            (else (display "ongeldige boodschap - kogels"))))

    dispatch-kogels))