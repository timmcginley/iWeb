
(defun tplaatser (xw yw hoogte invul / plaatspunt)
	(setq plaatspunt ( list (+ (nth 0 refpunt )xw ) (+ (nth 1 refpunt )yw )   ) )
	(setvar "textstyle" "standard")
	(command "text" plaatspunt hoogte "0" invul)
    	)

(defun rtd (a)
  	(* a (/ 180 pi))
  	)
(defun dtr (a)
  	(* pi (/ a 180))
  	)
(defun tkader (xw1 yw1 / kpunt )
	(setq kpunt (list (+ (nth 0 refpunt ) xw1 ) (+ (nth 1 refpunt ) yw1) ) ) 
	(command "rectang" kpunt "@800,400")
	)
(defun tkader2 (xw1 yw1 xw2 yw2 / kpunt kpunt2 )
	(setq 
	kpunt (list (+ (nth 0 refpunt ) xw1 ) (+ (nth 1 refpunt ) yw1) )  
	kpunt2 (strcat "@" (rtos  xw2  2 0 )"," (rtos  yw2 2 0 ) ) ) 
	(command "rectang" kpunt kpunt2)
	)
(defun block_controle ()
  	(if (= nil (tblsearch "block" blnaam))
     	(setq blnaam (strcat "c:\\pex\\" blnaam))
	)
  	)

(defun plaatsing ()
	(setq refpunt (getpoint"\nEnter a point where you want to place the drawing frame. "))
	(setq cutclr (getstring "\n Enter color code <A=2, B=3, C=4, D=5 or E=6>: "))
	(setq 	eltWord (getstring "\n Enter the secret word of the day: ")
  		textLayer (strcat "08-text." eltWord)
      		connectLayer (strcat "08-connection." eltWord)
    		markerLayer	(strcat "08-coord.marker." eltword)
      		cutmarkerLayer (strcat "08-coord.marker.cut." eltword)
				       )
  	)

(defun xploder()
	(setq 	ct 0
		xpl (ssget "X" '((0 . "INSERT"))) )
	(if (/= nil xpl)  
       		(repeat (sslength xpl)
    		(command "explode" (ssname xpl ct))
  		(setq ct (+ 1 ct))
	      		)
	  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;	da unroller
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun unroller ()

  ;;init 
  	(command "purge" "b" "catTot" "n")
  	(command "purge" "b" "cutTot" "n")
  	(command "purge" "b" "ss" "n")
  	(command "ucs" "w")
  	(setq refpuntss (list (- (nth 0 refpunt) 500) (+ (nth 1 refpunt) 300) (nth 2 refpunt)))


  
 

    

;; selection entry	
	(setvar "osmode" 1)
  	(setq	catclr 1
    		name (getstring "\n Enter the element code:")
		)

  	(print "Select carrier triangle elements: ")
		(setq cat (ssget))

  	(print "Select curve triangle elements: ")    
	(setq 	cut (ssget)

		pt1 (getpoint "\n Enter base point [A]: ")
		pt2 (getpoint "\n Enter wide angle point of the carrier triangle [C]:")
		pt3 (getpoint "\n Enter sharp angle point of the carrier triangle [B]: ")
		pt4 (getpoint "\n Enter end point of the curve triangle [D]: ")
		)

;;the nitty gritty

  	;; color-code the difference between the carrier and the curve triangle
  	(command "chprop" cut "" "c" cutclr "")

  	;;turn osnap off
  	(setvar "osmode" 0)
  


  
	;;unroll carrier triangle
    	(setq	catTot (ssadd)
		cutTot (ssadd)
		cTot (ssadd))
 
  
  
  	(command "ucs" "3" pt2 pt3 pt1)
  	(command "ucs" "x" "180")
   

	(setq
    		pta (trans pt1 0 1)
		ptb (trans pt3 0 1)
		ptc (trans pt2 0 1)
		pta1 (list (+ (nth 0 pta) 25) (+ (nth 1 pta) 50)(nth 2 pta) )
		ptb1 (list (- (nth 0 ptb) 50) (+ (nth 1 ptb) 25)(nth 2 ptb) )
		ptc1 (list (+ (nth 0 ptc) 50) (- (nth 1 ptc) 50)(nth 2 ptc) )
		ptc2 (list (+ (nth 0 ptc) 300) (- (nth 1 ptc) 150)(nth 2 ptc) )
	 )


  	;; make and insert block
		(command "text" ptc2 64 "0" (strcat name))
	  	(ssadd (entlast) catTot)
	(setq ct 0)
	
	(repeat (sslength cat)
		(command "copy" (ssname cat ct) "" (list 0 0 0) (list 0 0 0) )
  		(ssadd (entlast) cTot)
		(ssadd (ssname cat ct) catTot)
	  	(setq ct (+ 1 ct) )
		)
	(command "copy" catTot "" (list 0 0 0) (list 0 0 0))

	  	;; place abc
	  
	  	(setvar "clayer" textLayer)
	  	(command "text" pta1 36 "0" "A")
	  	(ssadd (entlast) catTot)

	  	;;(command "text" ptc2 64 "0" (strcat name))
	  	;;(ssadd (entlast) catTot)
	  
	  	(command "text" ptb1 36 "0" "B")
		(ssadd(entlast) catTot)
		(command "text" ptc1 36 "0" "C")
		(ssadd(entlast) catTot)






  	(command "block" "catTot" "0,0,0" catTot "")
	(command "ucs" "w")
  	(command "-insert" "catTot" refpunt "1" "1" "")
  	(command "explode" (entlast))	

  	;;unroll curve triangle

	(setq ct 0)
	
	(repeat (sslength cut)
		(command "copy" (ssname cut ct) "" (list 0 0 0) (list 0 0 0) )
  		(ssadd (entlast) cTot)
	  	(ssadd (ssname cut ct) cutTot)
		(setq ct (+ 1 ct))
		)
	(command "copy" cutTot "" (list 0 0 0) (list 0 0 0) )

  		(command "ucs" "3" pt2 pt3 pt4)
		(setq 	ptd (trans pt4 0 1)
			ptd1 (list (- (nth 0 ptd) 25) (- (nth 1 ptd) 50)(nth 2 ptd) )
			)
	  	(command "text" ptd1 36 "0" "D")
	  	(ssadd (entlast) cutTot)
  	
  	(command "block" "cutTot" "0,0,0" cutTot "")
  	(command "ucs" "w")
  	(command "-insert" "cutTot" refpunt "1" "1" "")

  	(command "explode" (entlast))

	;;position angular view
  
	
 	(command "ucs" "3" pt2 pt3 pt1)
  	(command "ucs" "y" "-90")
  	(command "ucs" "z" "90")
  	(setq 	pt1_u (trans pt1 0 1)
		pt1_t (list (nth 0 pt1_u) (+ (nth 1 pt1_u) 12)(nth 2 pt1_u) )
		pt2_u (trans pt2 0 1)
		pt2_t (list (nth 0 pt2_u) (+ (nth 1 pt2_u) 12)(nth 2 pt2_u) )
		pt3_u (trans pt3 0 1)
		pt3_t (list (nth 0 pt3_u) (+ (nth 1 pt3_u) 48)(nth 2 pt2_u) )
		pt4_u (trans pt4 0 1)
		pt4_t (list (nth 0 pt4_u) (+ (nth 1 pt4_u) 12)(nth 2 pt4_u) )

		angl1 (rtd (angle  (list 0 0 0) pt4_u))
		  )
  	(command "text" pt1_t 36 "90" "A")
  		(ssadd (entlast) cTot)
  	(command "text" pt2_t 36 "90" "B")
  		(ssadd (entlast) cTot)
  	(command "text" pt3_t 36 "90" "C")
  		(ssadd (entlast) cTot)
  	(command "text" pt4_t 36 "90" "D")
  		(ssadd (entlast) cTot)
	(command "block" "ss" "0,0,0" cTot "")

  	(command "ucs" "w")
	(command "-insert" "ss" refpuntss "1" "1" "-90")
 	(command "explode" (entlast) )

  	;;place text
  	(tplaatser -400 +350 12 (strcat "hoek A-D loodrecht over BC: " ) )
  	(tplaatser -400 +276 24 (strcat (rtos angl1 2 0) " deg." ) )
	(tkader -900 -600 )
	(tplaatser -650 -400 24 (strcat name) )
	(tplaatser -800 -480 12 (strcat "A (x,y,z)=")) 
	(tplaatser -800 -500 12 (strcat
		 	"x: "  (rtos (nth 0 pt1) 2 0 ) 
		 	", y: "  (rtos (nth 1 pt1) 2 0 ) 
		 	", z: "  (rtos (nth 2 pt1) 2 0 )
				)
  			)
	(tplaatser -800 -280 12 (strcat "C (x,y,z)="))   
	(tplaatser -800 -300 12 (strcat
		    	"x: "  (rtos (nth 0 pt2) 2 0 ) 
		 	", y: "  (rtos (nth 1 pt2) 2 0 ) 
		 	", z: "  (rtos (nth 2 pt2) 2 0 )
				)
  			)
	(tplaatser -500 -480 12 (strcat "B (x,y,z)=")) 
	(tplaatser -500 -500 12 (strcat
		    	"x: "  (rtos (nth 0 pt3) 2 0 ) 
		 	", y: "  (rtos (nth 1 pt3) 2 0 ) 
		 	", z: "  (rtos (nth 2 pt3) 2 0 ) ))
	(tplaatser -500 -280 12 (strcat "D (x,y,z)="))
	(tplaatser -500 -300 12 (strcat
		       	"x: "  (rtos (nth 0 pt4) 2 0 ) 
		 	", y: "  (rtos (nth 1 pt4) 2 0 ) 
		 	", z: "  (rtos (nth 2 pt4) 2 0 ) ))


	;; connection line
  	(command "line" (c:cal "plt(pt1,pt4,0.5)") (list (- (nth 0 refpunt) 900)(nth 1 refpunt)(nth 2 refpunt)) "")
  	(command "chprop" (entlast) "" "la" connectLayer "")


  ;; end unroller
  
	(setvar "clayer" old_layer)
	(setvar "osmode" oldsnap)
	(setq refpunt (list (+ (nth 0 refpunt) 5000) (+ (nth 1 refpunt) 0) (nth 2 refpunt) ) )

  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;	da unroller without the unrolling part, so we're just placing the element
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun  placer1 ()

  ;;init
  	(print "placer")
  	(command "purge" "b" "Tot" "n")
    	(command "ucs" "w")
  	(setq refpuntss (list (- (nth 0 refpunt) 500) (+ (nth 1 refpunt) 300) (nth 2 refpunt)))


  
 

    

;; selection entry	
	(setvar "osmode" 1)
  	(setq	catclr 1
    		name (getstring "\n Enter the element code:")
		)

  	(print "Select the elements for a single entity: ")
		(setq cat (ssget)


		pt1 (getpoint "\n Enter base point [A]: ")
		pt2 (getpoint "\n Enter point [B]: ")
		pt3 (getpoint "\n Enter point [C]:")
		pt4 (getpoint "\n Enter point [D]: ")
		)

;;the nitty gritty

  	;; color-code the difference between the carrier and the curve triangle
  	(command "chprop" cat "" "c" cutclr "")

  	;;turn osnap off
  	(setvar "osmode" 0)
  


  
	;;unroll carrier triangle
    	(setq	Tot (ssadd)

		cTot (ssadd))
 
  
  
  	(command "ucs" "3" pt1 pt2 pt3)
  	
   

	(setq
    		pta (trans pt1 0 1)
		ptb (trans pt2 0 1)
		ptc (trans pt3 0 1)
		ptd (trans pt4 0 1)
		pta1 (list (+ (nth 0 pta) 25) (+ (nth 1 pta) 50)(nth 2 pta) )
		ptb1 (list (- (nth 0 ptb) 50) (+ (nth 1 ptb) 25)(nth 2 ptb) )
		ptc1 (list (+ (nth 0 ptc) 50) (- (nth 1 ptc) 50)(nth 2 ptc) )
		ptc2 (list (+ (nth 0 ptc) 300) (- (nth 1 ptc) 150)(nth 2 ptc) )
		ptd1 (list (- (nth 0 ptd) 25) (- (nth 1 ptd) 50)(nth 2 ptd) )	
		)

  	;; place abc
  	(setvar "clayer" textLayer)
  	(command "text" pta1 36 "0" "A")
  	(ssadd (entlast) Tot)

  	(command "text" ptc2 64 "0" (strcat name))
  	(ssadd (entlast) Tot)
  
  	(command "text" ptb1 36 "0" "B")
	(ssadd(entlast) Tot)
	(command "text" ptc1 36 "0" "C")
	(ssadd(entlast) Tot)
  	(command "text" ptd1 36 "0" "D")
  	(ssadd (entlast) Tot)

  	;; make and insert block
  	
	(setq ct 0)
	
	(repeat (sslength cat)
		(ssadd (ssname cat ct) Tot)
	  	(setq ct (+ 1 ct) )
		)
	(command "copy" Tot "" (list 0 0 0) (list 0 0 0))
	(command "block" "Tot" "0,0,0" Tot "")
	(command "ucs" "w")
  	(command "-insert" "Tot" refpunt "1" "1" "")
  	(command "explode" (entlast))	

  	
	(tkader -900 -600 )
	(tplaatser -650 -400 24 (strcat name) )
	(tplaatser -800 -480 12 (strcat "A (x,y,z)=")) 
	(tplaatser -800 -500 12 (strcat
		 	"x: "  (rtos (nth 0 pt1) 2 0 ) 
		 	", y: "  (rtos (nth 1 pt1) 2 0 ) 
		 	", z: "  (rtos (nth 2 pt1) 2 0 )
				)
  			)
	(tplaatser -800 -280 12 (strcat "C (x,y,z)="))   
	(tplaatser -800 -300 12 (strcat
		    	"x: "  (rtos (nth 0 pt3) 2 0 ) 
		 	", y: "  (rtos (nth 1 pt3) 2 0 ) 
		 	", z: "  (rtos (nth 2 pt3) 2 0 )
				)
  			)
	(tplaatser -500 -480 12 (strcat "B (x,y,z)=")) 
	(tplaatser -500 -500 12 (strcat
		    	"x: "  (rtos (nth 0 pt2) 2 0 ) 
		 	", y: "  (rtos (nth 1 pt2) 2 0 ) 
		 	", z: "  (rtos (nth 2 pt2) 2 0 ) ))
	(tplaatser -500 -280 12 (strcat "D (x,y,z)="))
	(tplaatser -500 -300 12 (strcat
		       	"x: "  (rtos (nth 0 pt4) 2 0 ) 
		 	", y: "  (rtos (nth 1 pt4) 2 0 ) 
		 	", z: "  (rtos (nth 2 pt4) 2 0 ) ))


	;; connection line
  	(command "line" (c:cal "plt(pt1,pt4,0.5)") (list (- (nth 0 refpunt) 900)(nth 1 refpunt)(nth 2 refpunt)) "")
  	(command "chprop" (entlast) "" "la" connectLayer "")


  ;; end unroller
  
	(setvar "clayer" old_layer)
	(setvar "osmode" oldsnap)
	(setq refpunt (list (+(nth 0 refpunt)0) (+ (nth 1 refpunt) 1500) (nth 2 refpunt)) )

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;	a placer of a number of segments
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun  placer (segs)

  ;;init
  	(print "placersegs")
  	(command "purge" "b" "Tot" "n")
    	(command "purge" "b" "marker" "n")
	(command "ucs" "w")
  	(setq refpuntss (list (- (nth 0 refpunt) 500) (+ (nth 1 refpunt) 300) (nth 2 refpunt))
		blnaam "marker")
	(block_controle)

  
 

    

;; selection entry	
	(setvar "osmode" 1)
  	(setq	catclr 1
    		name (getstring "\n Enter the element code:")
		)

  	(print "get elements: ")
		(setq cat (ssget)
		pt_i (getpoint "\n Enter the first point on the inside: ")
		pts (+ 1 segs)
		ct 0 )
  	(print segs)
  	(print pts)
	(repeat segs
		(setq 	pt (getpoint "\n next interior point: ")
			pt_i (append pt_i pt)
			ct (+ 1 ct) )
			)
		(setq 	pt_e (getpoint "\n Enter the first point on the outside: ")
			ct 0 )
	(repeat segs
		(setq 	pt (getpoint "\n next exterior point: ")
			pt_e (append pt_e pt)
			ct (+ 1 ct))
			)
				

;;the nitty gritty

  	;; color-code the difference between the carrier and the curve triangle
  	(command "chprop" cat "" "c" cutclr "")

  	;;turn osnap off
  	(setvar "osmode" 0)
  


  
	;;unroll carrier triangle
    	(setq	Tot (ssadd)
		cTot (ssadd)
		pt_i1 (list (nth 0 pt_i)(nth 1 pt_i)(nth 2 pt_i))
		pt_e1 (list (nth 0 pt_e)(nth 1 pt_e)(nth 2 pt_e))
		pt_i2(list (nth 3 pt_i)(nth 4 pt_i)(nth 5 pt_i)))

 	(command "ucs" "3" pt_i1 pt_e1 pt_i2)
	(command "ucs" "z" "-90")
	(command "ucs" "y" "180")
    	(command "ucs" "x" "180")
	;; place text
  	(setvar "clayer" textLayer)
  	(setq  ct 0
	       ctt 1)
  
	(repeat pts
	  	
		(setq
		ptx_i (list (nth ct pt_i)(nth (+ 1 ct) pt_i)(nth (+ 2 ct) pt_i) )
		ptx_e (list (nth ct pt_e)(nth (+ 1 ct) pt_e)(nth (+ 2 ct) pt_e) )
		
		pt_i_u (trans ptx_i 0 1)
		pt_e_u (trans ptx_e 0 1)

		pt_i_u_t (list  (nth 0 pt_i_u) (+ (nth 1 pt_i_u) 25) (nth 2 pt_i_u) )
		pt_e_u_t (list  (nth 0 pt_e_u) (- (nth 1 pt_e_u) 50) (nth 2 pt_e_u) )
			   
		text_i 	(strcat name "-i-" (rtos ctt 2 0) )
		text_e	(strcat name "-e-" (rtos ctt 2 0) )
		)
		(command "text" pt_i_u_t 36 "0" text_i)
  		(ssadd (entlast) Tot)
		
		(command "-insert" blnaam pt_i_u "1" "1" pt_e_u)
		(ssadd(entlast) Tot)
		
  		(command "text" pt_e_u_t 36 "0" text_e )
  		(ssadd (entlast) Tot)
		
		(command "-insert" blnaam pt_e_u "1" "1" pt_i_u)
		(ssadd(entlast) Tot)
		
		(setq ct (+ 3 ct)
		      ctt (+ 1 ctt))
  	)	

  	;; make and insert block
  	
	(setq ct 0)
	
	(repeat (sslength cat)
		(ssadd (ssname cat ct) Tot)
	  	(setq ct (+ 1 ct) )
		)
	(command "copy" Tot "" (list 0 0 0) (list 0 0 0))
	(command "block" "Tot" "0,0,0" Tot "")
	(command "ucs" "w")
  	(command "-insert" "Tot" refpunt "1" "1" "")
  	(command "explode" (entlast))	

  	(setq dy (- (+ (* pts 70) 150)) )
	(tkader2 -900 -200 800 dy )
	(tplaatser -800 -280 24 (strcat name) )
	(setq 	ct 0
		ctp 0
		pt 1)
	(repeat pts
		(setq ty (- -300 (* ctp 20)))
		(tplaatser -800 ty 12 (strcat name"-i-" (rtos pt 2 0) " (x,y,z)=")) 
		(tplaatser -800 (- ty 20) 12 (strcat
		 	"x: "  (rtos (nth ct pt_i) 2 0 ) 
		 	", y: "  (rtos (nth (+ 1 ct) pt_i) 2 0 ) 
		 	", z: "  (rtos (nth (+ 2 ct) pt_i) 2 0 )
				))
	(tplaatser -500 ty 12 (strcat name"-e-"(rtos pt 2 0)" (x,y,z)="))   
	(tplaatser -500 (- ty 20) 12 (strcat
		    	"x: "  (rtos (nth ct pt_e) 2 0 ) 
		 	", y: "  (rtos (nth (+ 1 ct) pt_e) 2 0 ) 
		 	", z: "  (rtos (nth (+ 2 ct) pt_e) 2 0 )
				))
			(setq 	ct (+ 3 ct)
				ctp (+ 3 ctp)		
				pt (+ 1 pt))	
					)
	;; connection line
  	(command "line" (c:cal "plt(pt_i1,pt_i2,0.5)") (list (- (nth 0 refpunt) 900)(nth 1 refpunt)(nth 2 refpunt)) "")
  	(command "chprop" (entlast) "" "la" connectLayer "")


  ;; end unroller
  
	(setvar "clayer" old_layer)
	(setvar "osmode" oldsnap)
	(setq refpunt (list (+(nth 0 refpunt)6000) (+ (nth 1 refpunt) 0) (nth 2 refpunt)) )

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;	unroll corner column
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hoekkolom()

  ;; init
  
  (setq refpuntss (list (- (nth 0 refpunt) 500) (+ (nth 1 refpunt) 300) (nth 2 refpunt)))
  (command "purge" "b" "at0" "n")
  (command "purge" "b" "at1" "n")
  (command "purge" "b" "ot0" "n")
  (command "purge" "b" "ot1" "n")
  (command "purge" "b" "wb" "n")
  (command "purge" "b" "Tot" "n")
  (command "purge" "b" "marker" "n")
  (command "ucs" "w")
  (setvar "osmode" 1)
  (setq kname (getstring "\n Enter the element code: "))
  
  ;; selection
  
  (print "Select the axis triangles (counterclockwise) and outer triangles: ")
	(setq ss (ssget)
	      at0 (ssname ss 0)
	      at1 (ssname ss 1)
	      ot0 (ssname ss 2)
	      ot0 (ssname ss 3))
  (print "Select the wing body: ")
  	(setq wb (ssget))
  (setq ax0 (getpoint "\n Enter base axis point: ")
	ax1 (getpoint "\n Enter top axis point: ")
	bda (getpoint "\n Enter base point A: ")
	bdb (getpoint "\n Enter base point B: ")
	bdc (getpoint "\n Enter base point C: ")
	bdd (getpoint "\n Enter base point D: ")
	wb0 (getpoint "\n Enter the lower outer point of the wing body : ")
	wb1 (getpoint "\n Enter the higher outer point of the wing body : ")
	ot0Tot (ssadd)
	ot1Tot (ssadd)
	at0Tot (ssadd)
	at1Tot (ssadd)
	wbTot (ssadd)
	Tot (ssadd)
	)
  	(command "copy" (ssname ss 0) "" "0,0,0" "0,0,0")
  	(ssadd (entlast) at0Tot)

  	(command "copy" (ssname ss 1) "" "0,0,0" "0,0,0")
  	(ssadd (entlast) at1Tot)

  	(command "copy" (ssname ss 2) "" "0,0,0" "0,0,0")
  	(ssadd (entlast) ot0Tot)

  	(command "copy" (ssname ss 3) "" "0,0,0" "0,0,0")
  	(ssadd (entlast) ot1Tot)

  
  	(command "copy" wb "" "0,0,0" "0,0,0")
  	(ssadd (entlast) wbTot)


  (setvar "clayer" textLayer)
  (setvar "osmode" 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text axis body 1, at1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (command "ucs" "3" bda ax0 ax1 )
	(setq bda_u (trans bda 0 1)
	      ax1_u (trans ax1 0 1)
	      ax0_u (trans ax0 0 1)
	      
  		bda_t (list (+ (nth 0 bda_u) 25) (+ (nth 1 bda_u) 25)(nth 2 bda_u) )
		ax1_t (list (- (nth 0 ax1_u) 25) (+ (nth 1 ax1_u) 25)(nth 2 ax1_u) )
		ax0_t (list (- (nth 0 ax0_u) 25) (- (nth 1 ax0_u) 50)(nth 2 ax0_u) )
	 )

 (setq blnaam "marker")
(block_controle)
  (command "-insert" blnaam ax0_u "1" "1" ax1_u)
  (ssadd(entlast) at1Tot)
  
  (command "-insert" blnaam ax1_u "1" "1" ax0_u)
  (ssadd(entlast) at1Tot)

  (command "text" bda_t 36 "0" "A")
	(ssadd(entlast) at1Tot)
 (command "text" ax1_t 36 "0" "CD")
	(ssadd(entlast) at1Tot)
 (command "text" ax0_t 36 "0" "AB")
	(ssadd(entlast) at1Tot)

 (command "text"  (c:cal "plt(ax0_u,ax1_u,0.5)") 36 "0" (strcat kname))
	(ssadd(entlast) at1Tot)
 
  (command "copy" at1Tot "" (list 0 0 0) (list 0 0 0))
  (command "ucs" "w")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text axis body 0, at0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(command "ucs" "3" ax0 bdb ax1 )
	
	(setq 	bdb_u (trans bdb 0 1)
	
	
		bdb_t (list (- (nth 0 bdb_u) 50) (+ (nth 1 bdb_u) 25)(nth 2 bdb_u) )
		
	)

(command "text" bdb_t 36 "0" "B")
	(ssadd(entlast) at0Tot)
	(command "copy" at0Tot "" (list 0 0 0) (list 0 0 0))
  (command "ucs" "w")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text outer body 1, ot1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(command "ucs" "3" ax1 bdd bda )
(command "ucs" "z" "180" )	
	
	(setq 	bdd_u (trans bdd 0 1)
		
	
		bdd_t (list (+ (nth 0 bdd_u) 25) (- (nth 1 bdd_u) 50)(nth 2 bdd_u) )
		
	)

(command "text" bdd_t 36 "0" "D")
	(ssadd(entlast) ot1Tot)
	(command "copy" ot1Tot "" (list 0 0 0) (list 0 0 0))
  (command "ucs" "w")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text outer body 0, ot0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(command "ucs" "3" ax1 bdc bdb )
(command "ucs" "x" "180" )	
	
	(setq 	bdc_u (trans bdc 0 1)
	
		bdc_t (list (- (nth 0 bdc_u) 50) (- (nth 1 bdc_u) 50)(nth 2 bdc_u) )
	)
		
(command "text" bdc_t 36 "0" "C")
	(ssadd(entlast) ot0Tot)
 	(command "copy" ot0Tot "" (list 0 0 0) (list 0 0 0))
  (command "ucs" "w")

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text wing body , wb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(command "ucs" "3" ax0 ax1 wb0 )
(command "ucs" "x" "180" )	
(command "ucs" "z" "-90" )
	
	(setq 	ax0_u (trans ax0 0 1)
		ax1_u (trans ax1 0 1)
		ax0_t (list (+ (nth 0 ax0_u) 25) (+ (nth 1 ax0_u) 50)(nth 2 ax0_u) )
		ax1_t (list (+ (nth 0 ax1_u) 25) (- (nth 1 ax1_u) 75)(nth 2 ax1_u) )
	)

  (command "copy" wbTot "" (list 0 0 0) (list 0 0 0))
(command "text" ax0_t 36 "0" "AB")
	(ssadd(entlast) wbTot)
  (command "text" (c:cal "plt(ax0_t,ax1_t,0.5)") 36 "0" (strcat "w-"kname))
	(ssadd(entlast) wbTot)
(command "text" ax1_t 36 "0" "CD")
	(ssadd(entlast) wbTot)
(command "block" "wb" "0,0,0" wbTot "")
  (command "ucs" "w")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unroll ot1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(command "ucs" "3" ax1 bda bdd )
(command "block" "ot1" "0,0,0" ot1Tot "")
  (command "ucs" "w")
(command "ucs" "3" ax1 bda ax0 )
(command "ucs" "x" "180" )

  (command "-insert" "ot1" "0,0,0" "1" "1" "")
	(ssadd (entlast) at1Tot)
(command "ucs" "w")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unroll ot0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(command "ucs" "3" ax1 bdb bdc "")
(command "block" "ot0" "0,0,0" ot0Tot "")
(command "ucs" "w")
 (command "ucs" "3" ax1 bdb ax0 "")
(command "ucs" "x" "180" )
  (command "-insert" "ot0" "0,0,0" "1" "1" "")
	(ssadd (entlast) at0Tot)
(command "ucs" "w")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unroll at1 at0 place at refpunt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(command "ucs" "3" ax0 ax1 bda "")
(command "ucs" "z" "-90" )

  (command "block" "at1" "0,0,0" at1Tot "")
  (command "ucs" "w")
(command "ucs" "3" ax0 ax1 bdb "")
(command "ucs" "x" "180" )
(command "ucs" "z" "-90" )
  (command "block" "at0" "0,0,0" at0Tot "")
(command "ucs" "w")
  	(command "-insert" "at0" refpunt "1" "1" "")
  	(command "explode" (entlast))
	(command "-insert" "at1" refpunt "1" "1" "")	
	(command "explode" (entlast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unroll wing body wb 300 besides refpunt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(command "-insert" "wb" (list (+ (nth 0 refpunt) 300) (nth 1 refpunt)(nth 2 refpunt) ) "1" "1" "")
	(command "explode" (entlast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; explode those dang nasty nested blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(xploder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; place angular view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (command "ucs" "w")
(command "ucs" "3" ax1 ax0 wb1)
(command "ucs" "y" "-90" )

  ;;text
  (setq 	bda_u (trans bda 0 1)
		bda_t (list (- (nth 0 bda_u) 25) (nth 1 bda_u)(nth 2 bda_u) )
		bdb_u (trans bdb 0 1)
		bdb_t (list (nth 0 bdb_u)(nth 1 bdb_u)(nth 2 bdb_u) )
    		bdc_u (trans bdc 0 1)
		bdc_t (list (- (nth 0 bdc_u) 25) (- (nth 1 bdc_u) 36)(nth 2 bdc_u) )
    		bdd_u (trans bdd 0 1)
		bdd_t (list (nth 0 bdd_u) (- (nth 1 bdd_u) 36)(nth 2 bdd_u) )
	)

(command "text" bda_t 36 "0" "A")
	(ssadd(entlast) Tot)

(command "text" bdb_t 36 "0" "B")
	(ssadd(entlast) Tot)

(command "text" bdc_t 36 "0" "C")
	(ssadd(entlast) Tot)

(command "text" bdd_t 36 "0" "D")
	(ssadd(entlast) Tot)

  (command "block" "Tot" "0,0,0" ss wb Tot "")
  (command "ucs" "w")
  (setq ins (list (- (nth 0 refpunt) 600) (+(nth 1 refpunt)300)(nth 2 refpunt) ))
(command "-insert" "Tot" ins "1" "1" "")

(command "explode" (ssget "X" '((2 . "Tot")) ) )
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; place coordinate text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		

  
(tkader -900 -600 )

(tplaatser -650 -400 24 (strcat "Kolom: "kname) )

(tplaatser -800 -480 12 (strcat "AB (x,y,z)=")) 
(tplaatser -800 -500 12 (strcat
		 	"x: "  (rtos (nth 0 ax0) 2 0 ) 
		 	", y: "  (rtos (nth 1 ax0) 2 0 ) 
		 	", z: "  (rtos (nth 2 ax0) 2 0 )
			)
  )
(tplaatser -800 -280 12 (strcat "CD (x,y,z)="))   
(tplaatser -800 -300 12 (strcat
		    	"x: "  (rtos (nth 0 ax1) 2 0 ) 
		 	", y: "  (rtos (nth 1 ax1) 2 0 ) 
		 	", z: "  (rtos (nth 2 ax1) 2 0 )
			)
  )


(command "line" (c:cal "plt(ax0,ax1,0.5)") (list (- (nth 0 refpunt) 900)(nth 1 refpunt)(nth 2 refpunt)) "")
  (command "chprop" (entlast) "" "la" connectLayer "")
  (setvar "clayer" old_layer)
 (setvar "osmode" oldsnap)
  (setq refpunt (list (+(nth 0 refpunt)2500)  (nth 1 refpunt)  (nth 2 refpunt)) )
 (command "purge" "b" "at0" "n")
  (command "purge" "b" "at1" "n")
  (command "purge" "b" "ot0" "n")
  (command "purge" "b" "ot1" "n")
  (command "purge" "b" "wb" "n")
  (command "purge" "b" "Tot" "n")
  (command "ucs" "w")


  
	      
	      
;;end hoekkolom()
  
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;	main
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:unroll(/ kies xw yw xw1 yw1 )
  (setq 
	old_layer (getvar "clayer")
	oldsnap (getvar "osmode")
	)
  
(if (= nil refpunt ) (plaatsing))
(if (= nil textLayer ) (plaatsing))
(if (= nil connectLayer ) (plaatsing))
(if (= nil markerLayer ) (plaatsing))
(if (= nil cutmarkerLayer ) (plaatsing))	

  (if (= nil (tblsearch "layer" textLayer))
  	(command "layer" "m" textLayer ""))
(if (= nil (tblsearch "layer" connectLayer))  
       (command "layer" "m" connectLayer ""))
(if (= nil (tblsearch "layer" markerLayer))  
       (command "layer" "m" markerLayer ""))
(if (= nil (tblsearch "layer" cutmarkerLayer))  
       (command "layer" "m" cutmarkerLayer ""))
  
 

(initget "p P u U c C 1 3 6")
(setq kies (getkword "\nunroll GENERATOR < Place Unroll Corner-column 6-segmented_plate 3-segmented_plate 1-segment [Unroll] > "))
(cond
  ((= kies "p") (plaatsing))
  ((= kies "P") (plaatsing))
  ((= kies "u") (unroller))
  ((= kies "U") (unroller))
  ((= kies "c") (hoekkolom))
  ((= kies "C") (hoekkolom))
  ((= kies "6") (placer 6))
  ((= kies "3") (placer 3))
  ((= kies "1") (placer1))
  ( t (unroller))
)

)