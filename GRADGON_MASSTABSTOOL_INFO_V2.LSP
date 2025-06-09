;;; AutoLISP-Tool für Projektmaßstab und Höhenreduktion
;;; Name: GRADGON_MASSTABSTOOL.LSP
;;; Entwickelt für Nils Schneekloth
;;; Version: 3.3 (Kombinierter Befehl und Browser-Öffnung)

(vl-load-com) ; Lädt die Visual LISP Extensions

;;;--------------------------------------------------------------------
;;; Globale Variablen und Fehlerbehandlung
;;;--------------------------------------------------------------------
(setq *gradgon-old-error* *error*)
(defun *gradgon-error-handler* (msg)
  (if (not (member msg '("Funktion abgebrochen" "quit / exit abort")))
    (princ (strcat "\nFEHLER: " msg))
  )
  (setq *error* *gradgon-old-error*)
  (princ)
)

;; Globale Variablen
(setq *gradgon-m-factor* nil)
(setq *gradgon-inv-factor* nil)
(setq *gradgon-ref-x* nil)
(setq *gradgon-ref-y* nil)
(setq *gradgon-scale-info-mtext-handle* nil)

;;;--------------------------------------------------------------------
;;; Hilfsfunktionen
;;;--------------------------------------------------------------------

;;; Konvertiert ein Komma-String zu einer Gleitkommazahl
(defun gradgon:string-to-float-de (str / result)
  (if (and str (= (type str) 'STR) (> (strlen str) 0))
    (progn (setq str (vl-string-subst "." "," str)) (setq result (atof str)) result)
    0.0
  )
)

;;; Teilt einen String anhand eines Delimiters auf
(defun gradgon:split-string (str delim / pos res part)
  (setq res '())
  (while (setq pos (vl-string-search delim str))
    (setq part (substr str 1 pos)) (setq res (cons part res))
    (setq str (substr str (+ pos (strlen delim) 1)))
  )
  (setq res (cons str res)) (reverse res)
)

;;; Gibt das aktuelle Datum und die Uhrzeit als String zurück
(defun gradgon:get-current-datetime-string ()
  (setq cdate_val (getvar "CDATE")) (setq date_str (rtos cdate_val 2 6))
  (setq year (substr date_str 1 4)) (setq month (substr date_str 5 2))
  (setq day (substr date_str 7 2)) (setq hour (substr date_str 10 2))
  (setq minutes (substr date_str 12 2)) (setq seconds (substr date_str 14 2))
  (strcat day "." month "." year " " hour ":" minutes ":" seconds)
)

;;; Aktualisiert ein bestehendes MTEXT-Objekt oder erstellt ein neues
(defun gradgon:update-or-create-scale-info-text (basePoint scaleFactor mFactor / acadDoc mtextObj textString existingMtextObj existingMtextEname)
  (setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq textString (strcat
    "Skalierung angewendet am " (gradgon:get-current-datetime-string)
    "\nBenutzer: " (getvar "LOGINNAME")
    "\nBasispunkt: X=" (rtos (car basePoint) 2 3) ", Y=" (rtos (cadr basePoint) 2 3)
    "\nAngewandter Faktor (inv): " (rtos scaleFactor 2 10)
    "\nProjektmaßstab (m): " (rtos mFactor 2 10)
    "\nDieses Textobjekt dokumentiert die Skalierung. BITTE NICHT LÖSCHEN!"
  ))
  (setq existingMtextObj nil)
  (if (and *gradgon-scale-info-mtext-handle* (setq existingMtextEname (handent *gradgon-scale-info-mtext-handle*))
      (not (vl-catch-all-error-p (setq existingMtextObj (vlax-ename->vla-object existingMtextEname))))
    )
    (if (and existingMtextObj (not (vlax-object-released-p existingMtextObj)))
      (progn
        (vla-put-TextString existingMtextObj textString)
        (vla-put-InsertionPoint existingMtextObj (vlax-3d-point (list (+ (car basePoint) 10.0) (+ (cadr basePoint) 10.0) 0.0)))
        (princ "\nBestehendes Skalierungs-Info-Textobjekt aktualisiert.")
      )
      (setq *gradgon-scale-info-mtext-handle* nil)
    )
  )
  (if (not *gradgon-scale-info-mtext-handle*)
    (progn
      (setq mtextObj (vla-AddMText (vla-get-ModelSpace acadDoc) (vlax-3d-point (list (+ (car basePoint) 10.0) (+ (cadr basePoint) 10.0) 0.0)) 100.0 textString))
      (vla-put-Height mtextObj 5.0) (vla-put-Layer mtextObj "0")
      (setq *gradgon-scale-info-mtext-handle* (vla-get-handle mtextObj))
      (princ "\nNeues Skalierungs-Info-Textobjekt erstellt.")
    )
  )
)

;;; Löscht das MTEXT-Objekt
(defun gradgon:delete-scale-info-text (/ mtextEname mtextObj)
  (if (and *gradgon-scale-info-mtext-handle* (= (type *gradgon-scale-info-mtext-handle*) 'STR))
    (if (setq mtextEname (handent *gradgon-scale-info-mtext-handle*))
      (if (not (vl-catch-all-error-p (setq mtextObj (vlax-ename->vla-object mtextEname))))
        (if (and mtextObj (not (vlax-object-released-p mtextObj))) (vla-delete mtextObj))
      )
    )
  )
  (princ "\nSkalierungs-Info-Textobjekt gelöscht.")
  (setq *gradgon-scale-info-mtext-handle* nil)
)

;;;--------------------------------------------------------------------
;;; Log-Datei Funktionen (Stabiler Ersatz für XData)
;;;--------------------------------------------------------------------
(defun gradgon:get-log-filepath () (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) "_scale_info.log"))

;;; Speichert die Daten als robuste "Schlüssel-Wert"-Liste
(defun gradgon:save-scale-info (factorInv basePt dateStr userStr mFactor mtextHandleStr / logPath f data-list)
  (setq logPath (gradgon:get-log-filepath))
  (setq data-list
    (list
      (cons "factor" factorInv)
      (cons "mfactor" mFactor)
      (cons "baseX" (car basePt))
      (cons "baseY" (cadr basePt))
      (cons "date" dateStr)
      (cons "user" userStr)
      (cons "handle" mtextHandleStr)
    )
  )
  (if (setq f (open logPath "w"))
    (progn (prin1 data-list f) (close f) (princ (strcat "\nSkalierungs-Info in " (vl-filename-base logPath) ".log gespeichert.")))
    (alert (strcat "FEHLER: Konnte Log-Datei nicht schreiben:\n" logPath))
  )
)

;;; Lädt die Skalierungsinformationen aus der Log-Datei
(defun gradgon:load-scale-info (/ logPath f loadedData)
  (setq logPath (gradgon:get-log-filepath))
  (if (findfile logPath)
    (if (setq f (open logPath "r"))
      (progn (setq loadedData (read (read-line f))) (close f) (princ "\nSkalierungs-Info aus Log-Datei geladen.") loadedData)
      (progn (alert (strcat "FEHLER: Konnte Log-Datei nicht lesen:\n" logPath)) nil)
    )
    nil
  )
)

;;; Löscht die Log-Datei
(defun gradgon:delete-log-file (/ logPath)
  (setq logPath (gradgon:get-log-filepath))
  (if (findfile logPath)
    (if (vl-file-delete logPath)
      (princ "\nLog-Datei gelöscht.")
      (princ "\nFEHLER: Log-Datei konnte nicht gelöscht werden.")
    )
  )
)

;;;--------------------------------------------------------------------
;;; Hauptbefehle
;;;--------------------------------------------------------------------

;;; **NEU**: Kombinierter Befehl zum Laden und Anwenden
(defun c:GRADGON_SKALIEREN (/ csvFilePath fileHandle line contentList headers headerMap dataLine idx ss basePt dateStr userStr)
  (setq *error* *gradgon-error-handler*)
  (princ "\n--- Skalierung anwenden (Laden + Skalieren) ---")
  
  ;; --- TEIL 1: CSV LADEN ---
  (if (findfile (gradgon:get-log-filepath))
    (progn (alert "FEHLER: Es wurde bereits eine Skalierung angewendet.\nBitte zuerst 'GRADGON_ZURUECKSETZEN' ausführen.") (exit))
  )
  
  (setq csvFilePath (getfiled "CSV-Datei mit Skalierungsfaktoren auswählen" (getvar "DWGPREFIX") "csv" 16))
  (if (not csvFilePath)
    (progn (princ "\nDateiauswahl abgebrochen.") (setq *error* *gradgon-old-error*) (exit))
  )
  
  (if (setq fileHandle (open csvFilePath "r"))
    (progn
      (if (setq line (read-line fileHandle))
        (progn
          (setq headers (gradgon:split-string line ";")) (setq headerMap '() idx 0)
          (foreach h headers (setq headerMap (cons (cons (strcase h) idx) headerMap)) (setq idx (1+ idx)))
          (if (and (assoc "PROJEKTMASSSTAB" headerMap) (assoc "MASSSTABSANPASSUNG" headerMap) (assoc "RECHTSWERT" headerMap) (assoc "HOCHWERT" headerMap))
            (if (setq dataLine (read-line fileHandle))
              (progn
                (setq contentList (gradgon:split-string dataLine ";"))
                (setq *gradgon-m-factor* (gradgon:string-to-float-de (nth (cdr (assoc "PROJEKTMASSSTAB" headerMap)) contentList)))
                (setq *gradgon-inv-factor* (gradgon:string-to-float-de (nth (cdr (assoc "MASSSTABSANPASSUNG" headerMap)) contentList)))
                (setq *gradgon-ref-x* (gradgon:string-to-float-de (nth (cdr (assoc "RECHTSWERT" headerMap)) contentList)))
                (setq *gradgon-ref-y* (gradgon:string-to-float-de (nth (cdr (assoc "HOCHWERT" headerMap)) contentList)))
                (princ (strcat "\nFaktoren aus CSV geladen:"
                               "\n  Projektmaßstab (m): " (rtos *gradgon-m-factor* 2 8)
                               "\n  Maßstabsanpassung (inv): " (rtos *gradgon-inv-factor* 2 8)
                               "\n  Referenzpunkt X: " (rtos *gradgon-ref-x* 2 3)
                               "\n  Referenzpunkt Y: " (rtos *gradgon-ref-y* 2 3)))
              )
              (progn (alert "Fehler: Keine Datenzeile in der CSV-Datei gefunden.") (setq *error* *gradgon-old-error*) (exit))
            )
            (progn (alert "Fehler: Benötigte Spalten nicht gefunden.\nErforderlich: PROJEKTMASSSTAB;MASSSTABSANPASSUNG;RECHTSWERT;HOCHWERT") (setq *error* *gradgon-old-error*) (exit))
          )
        )
        (progn (alert "Fehler: Leere CSV-Datei oder Kopfzeile konnte nicht gelesen werden.") (setq *error* *gradgon-old-error*) (exit))
      ) (close fileHandle)
    )
    (progn (alert (strcat "Fehler: Datei konnte nicht geöffnet werden:\n" csvFilePath)) (setq *error* *gradgon-old-error*) (exit))
  )

  ;; --- TEIL 2: SKALIERUNG ANWENDEN ---
  (if (not (= (getvar "INSUNITS") 6))
    (if (not (y-or-n-p (strcat "ACHTUNG: Zeichnungseinheiten sind nicht auf Meter eingestellt (" (itoa (getvar "INSUNITS")) ").\nFortfahren? (J/N) ")))
      (progn (princ "\nBefehl abgebrochen.") (setq *error* *gradgon-old-error*) (exit))
    )
  )
  (if (setq ss (ssget "\nObjekte zur Skalierung auswählen: "))
    (progn
      (vla-startundomark (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq basePt (list *gradgon-ref-x* *gradgon-ref-y* 0.0))
      (princ (strcat "\nWende Skalierung an mit Faktor " (rtos *gradgon-inv-factor* 2 10) "..."))
      (command "_.scale" ss "" "_none" basePt *gradgon-inv-factor*)
      (setq dateStr (gradgon:get-current-datetime-string))
      (setq userStr (getvar "LOGINNAME"))
      (gradgon:update-or-create-scale-info-text basePt *gradgon-inv-factor* *gradgon-m-factor*)
      (gradgon:save-scale-info *gradgon-inv-factor* basePt dateStr userStr *gradgon-m-factor* *gradgon-scale-info-mtext-handle*)
      (vla-endundomark (vla-get-ActiveDocument (vlax-get-acad-object)))
      (princ "\nSkalierung erfolgreich angewendet.")
    )
    (princ "\nKeine Objekte ausgewählt. Befehl abgebrochen.")
  )
  
  (setq *error* *gradgon-old-error*) (princ)
)


;;; Befehl: GRADGON_ZURUECKSETZEN
(defun c:GRADGON_ZURUECKSETZEN (/ ss loadedXData invFactor basePt resetFactor)
  (setq *error* *gradgon-error-handler*)
  (princ "\n--- Skalierung zurücksetzen ---")
  (if (setq loadedXData (gradgon:load-scale-info))
    (progn
      (setq invFactor   (cdr (assoc "factor" loadedXData)))
      (setq basePt      (list (cdr (assoc "baseX" loadedXData)) (cdr (assoc "baseY" loadedXData)) 0.0))
      (setq *gradgon-scale-info-mtext-handle* (cdr (assoc "handle" loadedXData)))
      (setq resetFactor (/ 1.0 invFactor))
      (princ (strcat "\nGeladene Skalierungsdaten:"
                     "\n  Angew. Faktor (inv): " (rtos invFactor 2 10)
                     "\n  Basispunkt: (" (rtos (car basePt) 2 3) ", " (rtos (cadr basePt) 2 3) ")"))
      (if (setq ss (ssget "\nObjekte zur Rückskalierung auswählen: "))
        (progn
          (vla-startundomark (vla-get-ActiveDocument (vlax-get-acad-object)))
          (princ (strcat "\nWende Rückskalierung an mit Faktor " (rtos resetFactor 2 10) "..."))
          (command "_.scale" ss "" "_none" basePt resetFactor)
          (gradgon:delete-scale-info-text)
          (gradgon:delete-log-file)
          (vla-endundomark (vla-get-ActiveDocument (vlax-get-acad-object)))
          (princ "\nSkalierung erfolgreich zurückgesetzt.")
        )
        (princ "\nKeine Objekte ausgewählt. Befehl abgebrochen.")
      )
    )
    (princ "\nKeine gespeicherten Skalierungsdaten gefunden. Nichts zum Zurücksetzen.")
  )
  (setq *error* *gradgon-old-error*) (princ)
)

;;; **MODIFIZIERT**: Befehl: GRADGON_INFO
(defun c:GRADGON_INFO (/ returnCoord xCoord yCoord msg url)
  (setq *error* *gradgon-error-handler*)
  (princ "\n--- Punktinformationen abfragen ---")
  (if (= (getvar "INSUNITS") 6)
    (princ "\nZeichnungseinheiten: Meter (OK)")
    (princ (strcat "\nACHTUNG: Zeichnungseinheiten sind nicht auf Meter eingestellt (" (itoa (getvar "INSUNITS")) ")."))
  )
  (if (setq returnCoord (getpoint "\nBitte einen Punkt in der Zeichnung auswählen: "))
    (progn
      (setq xCoord (car returnCoord)) (setq yCoord (cadr returnCoord))
      (setq msg (strcat "\nAusgewählte Koordinate:"
                        "\n  Rechtswert (X): " (rtos xCoord 2 3) " m"
                        "\n  Hochwert (Y):   " (rtos yCoord 2 3) " m"))
      (alert msg)
      ;; **NEU**: Webseite im Standardbrowser öffnen
      (setq url "https://sites.google.com/view/gradgon/etrs89-projektma%C3%9Fstab")
      (startapp "explorer" url)
      (princ (strcat "\nWebseite wird geöffnet: " url))
    )
    (princ "\nKein Punkt ausgewählt. Befehl abgebrochen.")
  )
  (setq *error* *gradgon-old-error*) (princ)
)

(princ "\n*** GRADGON_MASSTABSTOOL.LSP (Version 3.3) erfolgreich geladen. ***")
(princ "\nVerfügbare Befehle:")
(princ "\n  GRADGON_SKALIEREN     - Hauptbefehl: Lädt CSV und wendet Skalierung an.")
(princ "\n  GRADGON_ZURUECKSETZEN - Setzt eine angewendete Skalierung zurück.")
(princ "\n  GRADGON_INFO          - Zeigt Punktkoordinaten an und öffnet die Webseite.")
(princ)