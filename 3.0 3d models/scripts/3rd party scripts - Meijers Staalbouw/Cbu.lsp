;;;CBU  can copy entitys from a 3d model to a given point on the XY plane of the WCS
;;;By selecting 3 or more plane defenition points and the entitys itself.
;;;After copying these elements CBU places the original 3D coordinates of the defenition points
;;;in a text block.
;;;This textblock will also contain XDATA. This XDATA makes it possible to put entitys back to
;;;where they came from
;;;The first 3 definition points are important points. They define the temprarilly UCS by the
;;;3P option of the UCS command.
;;;The first point determines the origin
;;;The second a point on the postive x-axis
;;;The third point detemines a point on the positive Y-portion of the XY plane

;;;get x-data from ent 
(defun gx (  ent ap / )
 (cdr (nth 1(nth 0 (cdr (assoc -3 (entget ent (list ap )))))))
)
;;;put x-data to ent   
(defun px ( ent ap kind val / exdata newent)
 (setq kind (strcase kind))
 (cond
   ( (= kind "STRING" ) (setq kind 1000))
   ( (= kind "REAL" ) (setq kind 1040))
   ( (= kind "INTEGER" ) (setq kind 1070))
   ( (= kind "LONG" ) (setq kind 1071))
   ( (= kind "3D" ) (setq kind 1010))
 )
 (setq elm (entget ent))
 (regapp ap)
 (setq exdata (list (list -3 (list ap (cons kind val))))) 
 (setq newent (append elm exdata))	
 (entmod newent)
)
;;; gets assoc value from ent 
(defun ensoc ( ent nest nr / waarde)
  (repeat nest (setq en (entnext en)))    
  (setq waarde (cdr (assoc nr (entget en))))    
)
;;;inserts a textblock with text FROMPOINT to TOPOINT
;;;attatch some x-data 
(defun ift (fr to nr / xp yp zp)
  (setq xp (rtos (nth 0 fr)))
  (setq yp (rtos (nth 1 fr)))
  (setq zp (rtos (nth 2 fr)))
  (command "insert" "c:\\cbu\\cor" to (getvar "dimscale") "" "" xp yp zp)
  (px (entlast) "aartsoft-volg" "integer" nr)
)
;;;Makes a list of definition points
(defun dlist ( / pnt deflist teller)
  (setq deflist nil)
  (setq teller 1)      
  (while 
    (setq pnt(getpoint (strcat "\nGive point " (itoa teller))))
    (setq deflist (append (list pnt) deflist))
    (setq teller (+ teller 1))
  )
  (setq deflist (reverse deflist))      
)

;;;creates a list of wcs coordinates from list of definition points in current UCS
(defun wlist ( ucslijst / pnt deflist teller)
      (setq wcslijst nil)
      (setq teller 0)
      (repeat (length ucslijst)
        (setq wcslijst (append (list(trans(nth teller ucslijst) 1 0)) wcslijst))
        (setq teller (+ teller 1))
      )
      (setq wcslijst (reverse wcslijst)) 
)
 
;;;Copy to ucs    Ctu
;;;Selecteer de gewenste elementen
;;;Definieer een vlak door het opgeven van 3 punten
;;;Geef een punt op in het WCS waar de geselecteerde elementen terecht moeten komen.


(defun ctu (/ p1 p2 p3 p4 elms teller ulijst plijst wlijst p5 inst )
  (setq plijst (dlist))
  (setq wlijst (wlist plijst))
  (IF  (< (length plijst) 3)
    (alert "there were not enough definition points found")
    (progn
      (setq p1 (trans (nth 0 plijst) 1 0))
      (setq p2 (trans (nth 1 plijst) 1 0))
      (setq p3 (trans (nth 2 plijst) 1 0))
      (command "purge" "b" "CTU" "n")
      (command "ucs" "w")
      (prompt "\nSelecteer de te kopieren elementen")
      (setq elms nil)
      (while (= nil elms)
       (setq elms (ssget ))
       (if (= nil elms) (alert "No entitys selected"))
      )
      (comMand "chprop" elms "" "c" "4" "")
      (setq teller 0)
      (setq old_elms (ssadd))
      (repeat (sslength elms)
        (command (entmake (entget(ssname elms teller))))
        (setq teller (+ teller 1)) 
        (setq old_elms (ssadd (entlast) old_elms))
      )
      (comMand "chprop" elms "" "c" "bylayer" "")
      (initget "Y y N N")
      (setq kies (getkword "\nDo you want to remove entitys in model ? [ Yes ,< NO >]"))
        (cond
          ((or (= kies "Y")(= kies "y")) (command "erase" old_elms ""))
          ((or (= kies "n")(= kies "N")) )
          ( T )
        )
      (command "ucs" "3" p1 p2 p3)
      (setq p4 (trans p1 0 1))
      ;;;conversion definition points to ucs of definition plane
      (setq teller 0)
      (setq ulijst nil)
      (repeat (length plijst)
        (setq ulijst (append (list(trans(nth teller wlijst) 0 1)) ulijst))
        (setq teller (+ teller 1))
      )
      (setq ulijst (reverse ulijst))
      (if 
        (= nil (tblsearch "BLOCK" "CTU")) 
        (command "block" "CTU" p4 elms "")
        (command "block" "CTU" "Y" p4 elms "") 
      )
      (command "ucs" "p")
      (command "ucs" "p")
      (setq ipoint (getpoint "\nGive a point where to place the selection "))
      (setvar "osmode" 0)
      (command "insert" "CTU" ipoint "" "" "" )
      (command "explode" "l" "")



      
      (setq teller 0)
      (repeat (length plijst)
        (setq p5  (nth teller ulijst))
        (setq inst(list(+ (nth 0 ipoint)(nth 0 p5)) (+(nth 1 ipoint)(nth 1 p5)) (+(nth 2 ipoint)(nth 2 p5))))
        (ift (nth teller plijst) inst teller)
        (if (= teller 0)
          (progn
            (px (entlast) "aartsoft-wm" "3d" p1)
            (px (entlast) "aartsoft-wu" "3d" (trans inst 1 0))
          )
        )
        (if (= teller 1)
          (progn 
            (px (entlast) "aartsoft-wm" "3d" p2)
            (px (entlast) "aartsoft-wu" "3d" (trans inst 1 0))
          )
        )     
        (if (= teller 2) 
          (progn
            (px (entlast) "aartsoft-wm" "3d" p3)
            (px (entlast) "aartsoft-wu" "3d" (trans inst 1 0))
          )
        )
        (setq teller (+ teller 1))
      )
      (setvar "osmode" oldsnap)     
    )
  ) 
) 
;;;back routine
(defun back ( / elms xelms nelms teller nummer wm1 wu1 wm2 wu2 wm3 wu3 p4)
  ;;;selecting elements and finding xdata
  (prompt"Select all entitys to put them back from whetre they came from ")
  (setq elms (ssget))
  (if (or   (= elms nil)  (< (sslength elms) 3)    )
    (alert "There were not enough definition points selected")
    (progn
      (setq xelms (ssget "P" '( (-3  ("aartsoft-wu")))))
      (if (or   (= xelms nil)  (< (sslength xelms) 3)    )
        (alert "There were not enough definition points selected")
        (progn
          (setq teller 0)
          (repeat (sslength xelms)
            (setq nummer (gx (ssname xelms teller) "aartsoft-volg"))
            (cond
              ((= nummer 0)
               (progn
                 (setq wm1 (gx (ssname xelms teller) "aartsoft-wm"))
                 (setq wu1  (cdr (assoc 10 (entget(ssname xelms teller)))))
                 (setq tv1  (cdr (assoc 210 (entget(ssname xelms teller)))))
                 (setq wu1  (trans wu1 tv1 0))
               ) 
              )
              ((= nummer 1)
               (progn
                 (setq wm2 (gx (ssname xelms teller) "aartsoft-wm"))
                 (setq wu2  (cdr (assoc 10 (entget(ssname xelms teller)))))
                 (setq tv2  (cdr (assoc 210 (entget(ssname xelms teller)))))
                 (setq wu2  (trans wu2 tv2 0))
               )   
              )
              ((= nummer 2)
               (progn
                 (setq wm3 (gx (ssname xelms teller) "aartsoft-wm"))
                 (setq wu3  (cdr (assoc 10 (entget(ssname xelms teller)))))
                 (setq tv3  (cdr (assoc 210 (entget(ssname xelms teller)))))
                 (setq wu3  (trans wu3 tv3 0))
               )   
              )
            )
            (setq teller (+ teller 1))
          )
          ;;;;copy procedure
          ;;;;remove coordinate blocks
          (command "select" elms "")
          (setq nelms (ssget "P" '( (-4 . "<NOT")(-3  ("aartsoft-volg")) (-4 . "NOT>"))))
          (setq elms nelms)

          (command "ucs" "w")
          (command "copy" elms "" (list 0 0 0) (list 0 0 0))
          (command "ucs" "3" wu1 wu2 wu3)
          (setq p4 (trans wu1 0 1))
             (if 
              (= nil (tblsearch "BLOCK" "CTU")) 
              (command "block" "CTU" p4 elms "")
              (command "block" "CTU" "Y" p4 elms "") 
             )
          (command "ucs" "p")
          (command "ucs" "3" wm1 wm2 wm3)
          (setq p4 (trans wm1 0 1))
          (setvar "osmode" 0)
          (command "insert" "CTU" p4 "" "" "" )
          (command "explode" (entlast)) 
          (setvar "osmode" oldsnap)
          (command "ucs" "P")
          (command "ucs" "p")
        )
      )
    )
  )  
)



;;;;;hoofd routine CBU
(defun c:cbu ()
  (setq oldsnap (getvar "osmode"))
  (initget "b B c C")
  (setq kies (getkword "\nBack/ <Copy> "))
  (cond
    ((or (= kies "B")(= kies "b")) (back))
    ((or (= kies "c")(= kies "C")) (ctu))
    ( T (Ctu))    
  )
)  