(deftemplate flokemon
    (slot name)
    (slot damage)
    (slot defense)
    (slot level)
    (slot burnDamage)
    (slot price)
    (slot type)
)

(defrule print-all-fire
	(flokemon (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burnDamage ?bd) (price ?p) (type ?t))
	=>
    (printout t "|" ?n "|" ?dmg "|" ?def "|" ?lvl "|" ?bd "|" ?p "|" crlf)
)

(defrule print-all-water
	(flokemon (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p) (type ?t))
    (test (eq ?t "Water"))
	=>
    (printout t "|" ?n "|" ?dmg "|" ?def "|" ?lvl "|" ?p "|" crlf)
)

(defrule print-fire
    (flokemon (type "Fire"))
    =>
    (printout t "FLokemon tipe Fire"))

/*Rapidash	500	50	10	10	50000
Magmar	400	120	60	40	200000
Growlithe	2100	150	65	60	300000
NineTales	1400	270	80	80	540000
Charmeleon	1200	250	85	90	230000

Vaporeon	900	120	45	80000
Staryu	800	70	30	50000
Poliwhirl	500	90	20	250000
Goldluck	2000	250	90	200000
Blastoise	1500	200	80	150000
*/

(deffunction clearScreen ()
	(for (bind ?i 0) (< ?i 25) (++ ?i)
    	(printout t crlf)
    )
)

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

(deffunction printFlokemonType(?type)
    (if (eq ?type "fire") then
  		(printout t "===============================================================================" crlf)
    	(printout t "|No. |Name                      |Damage    |Defense   |Level     |Price       |" crlf)
    	(printout t "===============================================================================" crlf)
    	(assert(flokemon (type "Fire")))
        
        (run)
    	(printout t "===============================================================================" crlf)  	    
    )
    
    (if (eq ?type "water") then
		(printout t "===============================================================================" crlf)
    	(printout t "|No. |Name                      |Damage    |Defense   |Level     |Price       |" crlf)
    	(printout t "===============================================================================" crlf)
        
        (run)
    	(printout t "===============================================================================" crlf)		        
    )
)

(deffunction viewFlokemon ()
    (bind ?choose 0)
    (while (neq ?choose 3)
    	(printout t "Choose Flokemon Type to view" crlf)
		(printout t "1. Fire Flokemon" crlf)
		(printout t "2. Water Flokemon" crlf)
		(printout t "3. Back" crlf)
		(printout t "Choose :" crlf)
        (bind ?choose (read))
        (if (eq (numberp ?choose) FALSE) 
            then (bind ?choose 0)
        elif(eq ?choose 1)
            then (printFlokemonType "fire")
        elif(eq ?choose 2)
            then (printFlokemonType "water")
        )
        (printout t "Press Enter To Continue..")
        (readline)
	)
)

(deffunction promptStringWithLength(?lowerBound ?upperBound ?message)
    (bind ?string "")
	(while (or (< (str-length ?string) ?lowerBound) (> (str-length ?string) ?upperBound))
        (printout t ?message)
        (bind ?string (readline))
        (if (eq (lexemep ?string) FALSE) 
            then (bind ?string "")
        )
    )
    (return ?string)
)

(deffunction promptIntegerWithLength(?lowerBound ?upperBound ?message)
    (bind ?num 0)
	(while (or (< ?num ?lowerBound) (> ?num ?upperBound))
        (printout t ?message)
        (bind ?num (read))
        (if (eq (numberp ?num) FALSE) 
            then (bind ?num 0)
        )
    )
    (return ?num)
)

(deffunction addFlokemonFire ()
    (bind ?name (promptStringWithLength 5 25 "Insert Flokemon Name [5 - 25 Character] : "))

    (bind ?damage (promptIntegerWithLength 10 5000 "Insert Flokemon Damage [10 - 5000] : "))
	
    (bind ?defense (promptIntegerWithLength 5 300 "Insert Flokemon Defense [5 - 300] : "))
	
    (bind ?level (promptIntegerWithLength 1 100 "Insert Flokemon Level [1 - 100] : "))        
    
    (bind ?price (promptIntegerWithLength 1000 1000000 "Insert Flokemon Price [1000 - 1000000] : "))        

    (bind ?burnDamage (promptIntegerWithLength 5 100 "Insert Flokemon Burn Damage [5 - 100] : "))
    
    (assert(flokemon(name ?name) (damage ?damage) (defense ?defense) (level ?level) (burnDamage ?burnDamage) (price ?price) (type "Fire")))
)

(deffunction addFlokemonWater ()
    (bind ?name (promptStringWithLength 5 25 "Insert Flokemon Name [5 - 25 Character] : "))

    (bind ?damage (promptIntegerWithLength 10 5000 "Insert Flokemon Damage [10 - 5000] : "))
	
    (bind ?defense (promptIntegerWithLength 5 300 "Insert Flokemon Defense [5 - 300] : "))
	
    (bind ?level (promptIntegerWithLength 1 100 "Insert Flokemon Level [1 - 100] : "))        
    
    (bind ?price (promptIntegerWithLength 1000 1000000 "Insert Flokemon Price [1000 - 1000000] : "))        

    (assert(flokemon(name ?name) (damage ?damage) (defense ?defense) (level ?level) (price ?price) (type "Water")))
)

(deffunction addFlokemon ()
    (bind ?choose 0)
    (while (neq ?choose 3)
    	(printout t "Choose Flokemon Type to add" crlf)
		(printout t "1. Fire Flokemon" crlf)
		(printout t "2. Water Flokemon" crlf)
		(printout t "3. Back" crlf)
		(printout t "Choose :" crlf)
        (bind ?choose (read))
        (if (eq (numberp ?choose) FALSE) 
            then (bind ?choose 0)
        elif(eq ?choose 1)
            then (addFlokemonFire)
        elif(eq ?choose 2)
            then (addFlokemonWater)
        )        
        (if (or (eq ?choose 1) (eq ?choose 2)) then (printout t "Flokemon successfully added..." crlf))
        (printout t "Press Enter To Continue..")
        (readline)
	)
)

(deffunction updateFlokemon ()
    (bind ?update 0)
)

(deffunction removeFlokemon ()
    (bind ?remove 0)
)

(deffunction findFlokemon ()
    (bind ?find 0)
)

(deffunction inserts()
    (assert (flokemon (name "Rapidash") (damage 500) (defense 50) (level 10) (burnDamage 10) (price 50000) (type "Fire")))
    (assert (flokemon (name "Magmar") (damage 400) (defense 120) (level 60) (burnDamage 40) (price 200000) (type "Fire")))
    (assert (flokemon (name "Goldluck") (damage 2000) (defense 250) (level 90) (price 200000) (type "Water")))
    (assert (flokemon (name "Blastoise") (damage 1500) (defense 200) (level 80) (price 150000) (type "Water")))
)

(deffunction mainFlokemon ()
    (inserts)
    (bind ?menus 0)
	(while (neq ?menus 6)
    	(clearScreen)
    	(menu)
    	(printout t "Choose : ")
    	(bind ?menus (read))
        (if (eq (numberp ?menus) FALSE) 
            then (bind ?menus 0)
        elif (eq ?menus 1) 
            then (viewFlokemon)
        elif (eq ?menus 2)
            then (addFlokemon)
        elif (eq ?menus 3) 
            then (updateFlokemon)
        elif (eq ?menus 4) 
            then (removeFlokemon)
        elif (eq ?menus 5) 
            then (findFlokemon)
        else
            (printout t "Thankyou..." crlf)
            (clear)
        )
	)
)

(mainFlokemon)



