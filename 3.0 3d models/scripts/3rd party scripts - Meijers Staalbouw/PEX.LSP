;;;Dit is een routine die snel een dxf a0kader insert
;;;het vervolgens explodeerd 
;;;vraagt om een aantal elementen
;;;vraagt om een insertion point 
;;;alle elementen in de laag 16030 zet
;;;alle geselecteerde elementen copieert naar het midden van het kader
;;;de gegevens in het dxf a0 kader invult (defoult vanuit de stukijst programmatuur)



(defun block_controle ()
 (if (= nil (tblsearch "block" blnaam))
     (setq blnaam (strcat "c:\\pex\\" blnaam))
 )
)

(defun laag_controle ()
 (if (= nil (tblsearch "layer" "16030"))  
       (command "layer" "m" "16030" "")
)
(if (= nil (tblsearch "layer" "16032"))  
       (command "layer" "m" "16032" "")    
)
)


(defun verplaatser (/ lijst punt plaatspunt teller)
  (setq teller 0)
  (setq old_osmode (getvar "osmode"))
  (setvar "osmode" 0)
  (setq plaatspunt refpunt)
  (prompt "\nSelect plate entitys ")
  (setq lijst (ssget))
  (if (/= nil lijst)
    (progn
      
      (setq punt (getpoint "\nGive a point in the somewhere middle of your selection "))
      (repeat (sslength lijst)
        (command "copy" (ssname lijst teller) "" punt plaatspunt)
        (command "chprop" (entlast) "" "La" "16030" "")
        (setq teller (+ teller 1))
      )
      (command "chprop" lijst "" "c" 4 "")
    )
    (alert "There was no selection made !!" )
  )
  (setvar "osmode" old_osmode)
  (setq lijst nil)
)


(defun xtoe ( soort geg / elm exdata newent)
        (setq elm (entget  (entlast)))        
        (regapp soort)
        (setq exdata (list (list -3 (list soort (cons 1000 geg))))) 
        (setq newent (append elm exdata))	
        (entmod newent)

)

(defun tplaatser (xwaarde ywaarde invul / plaatspunt  )
  (setvar "osmode" 0)
(setq plaatspunt (list (+ (nth 0 refpunt )xwaarde ) (+ (nth 1 refpunt )ywaarde ) 0))
(setq old_style (getvar "textstyle"))
(setq old_layer (getvar "clayer"))
(setvar "clayer" "16032")
(setvar "textstyle" "standard")
(command "text" plaatspunt 2.5 "" invul)
(setvar "textstyle" old_style)
(setvar "clayer" old_layer)
)


(defun teksthaler ()

    (setq pexnummer (getstring "\ngive the posnumber "))
    (setq pexaantal 1)



(setq pexdikte (getreal "\nGive the plate thickness "))
(if (= "" pexnummer) (alert "ALERT  !!!  posnumber = unknown "))
(if (= 0 pexaantal) (alert "ALERT !!! Quantity = 0 "))
(if (= 0 pexdikte) (alert "ALERT !!! Thickness = 0 "))


(alert (strcat
                "\n Posnumber : " pexnummer 
                "\n Quantity    : " (rtos pexaantal 2 0)
                "\n Thickness     : " (rtos pexdikte 2 0 )  
)
)

(tplaatser 0 0 (strcat "Stuknummer : " pexnummer ))
(xtoe "num" pexnummer)
(tplaatser 0 -12 (strcat "Aantal : 1"))
(tplaatser 0 -6 (strcat "Dikte : " (rtos pexdikte 2 0)))
(xtoe "dik" (rtos pexdikte 2 0))

)


(defun maken()
(setq old_osmode (getvar "osmode"))
(setvar "osmode" 0)

(setvar "osmode" old_osmode)
(setq pexinvoeg (list (+(nth 0 pexinvoeg)00) (+ (nth 1 pexinvoeg) 2000) (nth 2 pexinvoeg)))
(setq refpunt pexinvoeg) 
(teksthaler)
(verplaatser)
  (setvar "osmode" old_osmode)
)

(defun plaatsing ()
(setq pexinvoeg (getpoint"\nGive a point somewhere on the screen to place the plate. "))
)




(defun c:pex (/ kies pexnummer pexaantal pexdikte refpunt)
;;;let op de variabelen  PROJET,PROJECTNAAM en PEXPLACE zin globaal
;;;de variabelen paantal en pdikte komen uit het stuklijsten programma
(setq blnaam "pkader")
(setq pexnummer "1")
(block_controle)
(laag_controle)
(if (= nil pb-num) (setq pb-num 0))
(if (= nil paantal) (setq paantal 1))
(if (= nil pexinvoeg ) (plaatsing))
(setq pexdikte 0)

(initget "p P  m M")
(setq kies (getkword "\nDXFPLAAT GENERATOR < Place [Make] > "))
(cond
  ((= kies "p") (plaatsing))
  ((= kies "P") (plaatsing))
  ((= kies "m") (maken))
  ((= kies "M") (maken))
  ( t (maken))
)

)