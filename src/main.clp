(deffunction menu()
 	(printout t "+======================+" crlf)
	(printout t "| Flokemon Store       |" crlf)
	(printout t "+======================+" crlf)
	(printout t "1. View Flokemon        " crlf)
	(printout t "2. Add Flokemon         " crlf)
	(printout t "3. Update Flokemon      " crlf)
	(printout t "4. Remove Flokemon      " crlf)
	(printout t "5. Find Flokemon 	     " crlf)
	(printout t "6. Exit                 " crlf)
	(printout t "+======================+" crlf)   
)

(deffunction view ()
    
)

(deffunction add ()
    
)

(deffunction update ()
    
)

(deffunction remove ()
    
)

(deffunction find ()
    
)

(deffunction main ()
    (bind ?menus 0)
	(while (eq ?menus 6)
    	(menu)
    	(printout t "Choose : ")
    	(bind ?menus (read))
	)
)

(main)