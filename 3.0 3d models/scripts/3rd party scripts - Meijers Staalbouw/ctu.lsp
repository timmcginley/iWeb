;;;Copy to ucs    Ctu
;;;Selecteer de gewenste elementen
;;;Definieer een vlak door het opgeven van 3 punten
;;;Geef een punt op in het WCS waar de geselecteerde elementen terecht moeten komen.


(defun c:ctu ()

  (command "undo" "begin")
  (command "purge" "b" "CTU" "n")
  (command "ucs" "w")
  (prompt "\nSelecteer de te kopieren elementen")
  (setq elms (ssget ))
  (setq p1 (getpoint "\nGeef eeste punt van vlak"))
  (setq p2 (getpoint "\nGeef tweede punt van vlak"))
  (setq p3 (getpoint "\nGeef derde punt van vlak"))

  (command "copy" elms "" (list 0 0 0) (list 0 0 0))
  (command "ucs" "3" p1 p2 p3)
  (setq p4 (trans p1 0 1))

  (if 
    (= nil (tblsearch "BLOCK" "CTU")) 
    (command "block" "CTU" p4 elms "")
    (command "block" "CTU" "Y" p4 elms "") 
  )

  (command "ucs" "p")
  (command "ucs" "p")
  (command "insert" "CTU" pause "" "" "" )
  (command "rotate" "l" "" (getvar "lastpoint") "r" (getvar "lastpoint") pause pause)
  (command "explode" "l" "")
  (command "undo" "end")


)   