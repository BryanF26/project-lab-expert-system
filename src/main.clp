(deftemplate flokemonWater
    (slot name)
    (slot damage)
    (slot defense)
    (slot level)
    (slot price)
)

(deftemplate flokemonFire
    (slot name)
    (slot damage)
    (slot defense)
    (slot level)
    (slot burnDamage)
    (slot price)
)

(defglobal 
    ?*idx* = 1
    ?*totalFire* = 5
    ?*totalWater* = 5
)

(defrule print-all-fire
    (printFire)
	(flokemonFire (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burnDamage ?bd) (price ?p))
	=>
    (printout t "| " ?*idx* " | " ?n " | " ?dmg " | " ?def " | " ?lvl " | " ?bd " | " ?p " |" crlf)
    (++ ?*idx*)
)

(defrule print-all-water
    (printWater)
	(flokemonWater (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p))
	=>
    (printout t "| " ?*idx* " | " ?n " | " ?dmg " | " ?def " | " ?lvl " | " ?p " |" crlf)
    (++ ?*idx*)
)

(defrule update-fire
	(updateFire ?num ?name ?damage ?defense ?level ?price ?burnDamage)
    ?i <- (flokemonFire (name ?name) (damage ?damage) (defense ?defense) (level ?level) (burnDamage ?burnDamage) (price ?price))
	=>
    (bind ?is TRUE)
    (while (?is) 
        ()
    )
    (printout t ?num)
    (modify ?num (name ?name) (damage ?damage) (defense ?defense) (level ?level) (burnDamage ?burnDamage) (price ?price))
)

(defrule update-water
	(updateWater ?num ?name ?damage ?defense ?level ?price ?burnDamage)
    ?i <- (flokemonFire (name ?name) (damage ?damage) (defense ?defense) (level ?level) (burnDamage ?burnDamage) (price ?price))
	=>
    (bind ?is TRUE)
    (while (?is) 
        ()
    )
    (modify ?num (name ?name) (damage ?damage) (defense ?defense) (level ?level)  (price ?price))
    (modify ?num flokemonFire (name ?name) (damage ?damage) (defense ?defense) (level ?level) (burnDamage ?burnDamage) (price ?price))
)

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
        (printout t "Fire Flokemon List" crlf)
  		(printout t "=======================================================================" crlf)
    	(printout t "|No. |Name    |Damage    |Defense   |Level   |Burn Damage   |Price    |" crlf)
    	(printout t "=======================================================================" crlf)
    	(assert (printFire))
        (run)
        (retract-string "(printFire)")
        (facts)
    	(printout t "=======================================================================" crlf)  	    
    )
    
    (if (eq ?type "water") then
        (printout t "Water Flokemon List" crlf)
		(printout t "==========================================================" crlf)
    	(printout t "|No. |Name    |Damage    |Defense   |Level     |Price    |" crlf)
    	(printout t "==========================================================" crlf)
    	(assert (printWater))
        (run)
        (retract-string "(printWater)")
    	(printout t "==========================================================" crlf)		        
    )
    (bind ?*idx* 1)
)

(deffunction viewFlokemon ()
    (bind ?view 0)
    (while (neq ?view 3)
    	(printout t "Choose Flokemon Type to view" crlf)
		(printout t "1. Fire Flokemon" crlf)
		(printout t "2. Water Flokemon" crlf)
		(printout t "3. Back" crlf)
		(printout t "Choose :" crlf)
        (bind ?view (read))
        (if (eq (numberp ?view) FALSE) 
            then (bind ?view 0)
        elif(eq ?view 1)
            then (printFlokemonType "fire")
        elif(eq ?view 2)
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
    
    (++ ?*totalFire*)
    
    (assert(flokemonFire (name ?name) (damage ?damage) (defense ?defense) (level ?level) (burnDamage ?burnDamage) (price ?price)))
    
)

(deffunction addFlokemonWater ()
    (bind ?name (promptStringWithLength 5 25 "Insert Flokemon Name [5 - 25 Character] : "))

    (bind ?damage (promptIntegerWithLength 10 5000 "Insert Flokemon Damage [10 - 5000] : "))
	
    (bind ?defense (promptIntegerWithLength 5 300 "Insert Flokemon Defense [5 - 300] : "))
	
    (bind ?level (promptIntegerWithLength 1 100 "Insert Flokemon Level [1 - 100] : "))        
    
    (bind ?price (promptIntegerWithLength 1000 1000000 "Insert Flokemon Price [1000 - 1000000] : "))        

    (++ ?*totalWater*)

    (assert(flokemonWater (name ?name) (damage ?damage) (defense ?defense) (level ?level) (price ?price)))

)

(deffunction addFlokemon ()
    (bind ?add 0)
    (while (neq ?add 3)
    	(printout t "Choose Flokemon Type to add" crlf)
		(printout t "1. Fire Flokemon" crlf)
		(printout t "2. Water Flokemon" crlf)
		(printout t "3. Back" crlf)
		(printout t "Choose :" crlf)
        (bind ?add (read))
        (if (eq (numberp ?add) FALSE) 
            then (bind ?add 0)
        elif(eq ?add 1)
            then 
            (addFlokemonFire)
            (printout t "Flokemon successfully added..." crlf)
        	(printout t "Press Enter To Continue..")
            (readline)
            (printFlokemonType "fire")
        elif(eq ?add 2)
            then (addFlokemonWater)
 			(printout t "Flokemon successfully added..." crlf)
        	(printout t "Press Enter To Continue..")
            (readline)
            (printFlokemonType "water")
        )
        (printout t "Press Enter To Continue..")
        (readline)
	)
)

(deffunction updateFlokemonFire()
    (bind ?num 0)
    (while (or (< ?num 1) (> ?num ?*totalFire*))
        (printout t "Input flokemon number to be updated [1 - " ?*totalFire* "] : ")
        (bind ?num (read))
        (if (eq (numberp ?num) FALSE) then
            (bind ?num 0)
        )
    )
    (bind ?name (promptStringWithLength 5 25 "Insert Flokemon Name [5 - 25 Character] : "))

    (bind ?damage (promptIntegerWithLength 10 5000 "Insert Flokemon Damage [10 - 5000] : "))
	
    (bind ?defense (promptIntegerWithLength 5 300 "Insert Flokemon Defense [5 - 300] : "))
	
    (bind ?level (promptIntegerWithLength 1 100 "Insert Flokemon Level [1 - 100] : "))        
    
    (bind ?price (promptIntegerWithLength 1000 1000000 "Insert Flokemon Price [1000 - 1000000] : "))        
 
	(bind ?burnDamage (promptIntegerWithLength 5 100 "Insert Flokemon Burn Damage [5 - 100] : "))
    
    
    (assert (updateFire ?num ?name ?damage ?defense ?level ?price ?burnDamage))
    (run)
    (retract-string "(updateFire)")
)

(deffunction updateFlokemonWater()
    (bind ?num 0)
    (while (or (< ?num 1) (> ?num ?*totalWater*))
        (printout t "Input flokemon number to be updated [1 - " ?*totalWater* "] : ")
        (bind ?num (read))
        (if (eq (numberp ?num) FALSE) then
            (bind ?num 0)
        )
    )
    
    (bind ?num (+ (- ?*totalWater ?num) 1))
    
    (bind ?name (promptStringWithLength 5 25 "Insert Flokemon Name [5 - 25 Character] : "))

    (bind ?damage (promptIntegerWithLength 10 5000 "Insert Flokemon Damage [10 - 5000] : "))
	
    (bind ?defense (promptIntegerWithLength 5 300 "Insert Flokemon Defense [5 - 300] : "))
	
    (bind ?level (promptIntegerWithLength 1 100 "Insert Flokemon Level [1 - 100] : "))        
    
    (bind ?price (promptIntegerWithLength 1000 1000000 "Insert Flokemon Price [1000 - 1000000] : "))        
    
    (assert (updateWater ?num ?name ?damage ?defense ?level ?price))
    (run)
    (retract-string "(updateWater)")
)

(deffunction updateFlokemon ()
	(bind ?update 0)
    (while (neq ?update 3)
    	(printout t "Choose Flokemon Type to update" crlf)
		(printout t "1. Fire Flokemon" crlf)
		(printout t "2. Water Flokemon" crlf)
		(printout t "3. Back" crlf)
		(printout t "Choose :" crlf)
        (bind ?update (read))
        (if (eq (numberp ?update) FALSE) then 
            (bind ?update 0)
        elif(eq ?update 1) then
            (printFlokemonType "fire")
            (updateFlokemonFire)
            (printout t "Successfully update fire flokemon!" crlf)
            (printFlokemonType "fire")
        elif(eq ?update 2) then 
            (printFlokemonType "water")
            (updateFlokemonWater)
            (printout t "Successfully update water flokemon!" crlf)
            (printFlokemonType "water")
        )
        (printout t "Press Enter To Continue..")
        (readline)
	)
)

(deffunction removeFlokemonFire()
    (bind ?num 0)
    (while (or (< ?num 1) (> ?num ?*totalFire*))
        (printout t "Input flokemon number to be deleted [1 - " ?*totalFire* "] : ")
        (bind ?num (read))
        (if (eq (numberp ?num) FALSE) then
            (bind ?num 0)
        )
    )
	;(retract ?num (name ?name) (damage ?damage) (defense ?defense) (level ?level) (burnDamage ?burnDamage) (price ?price))
    (bind ?flok-fire (nth$ ?num flokemonFire))
    (printout t ?flok-fire:name crlf)
    (printout t ?flok-fire:damage crlf)
    (printout t ?flok-fire:defense crlf)
)

(deffunction removeFlokemonWater()
    (bind ?num 0)
    
    (while (or (< ?num 1) (> ?num ?*totalWater*))
        (printout t "Input flokemon number to be deleted [1 - " ?*totalWater* "] : ")
        (bind ?num (read))
        (if (eq (numberp ?num) FALSE) then
            (bind ?num 0)
        )
    )
        
    ;(retract ?num (name ?name) (damage ?damage) (defense ?defense) (level ?level)  (price ?price))
)

(deffunction removeFlokemon ()
    (bind ?remove 0)
    (while (neq ?remove 3)
    	(printout t "Choose Flokemon Type to removes" crlf)
		(printout t "1. Fire Flokemon" crlf)
		(printout t "2. Water Flokemon" crlf)
		(printout t "3. Back" crlf)
		(printout t "Choose :" crlf)
        (bind ?remove (read))
        (if (eq (numberp ?remove) FALSE) then 
            (bind ?remove 0)
        elif(eq ?remove 1) then
            (printFlokemonType "fire")
            (removeFlokemonFire)
            (printout t "Successfully deleted!" crlf)
            (printFlokemonType "fire")
        elif(eq ?remove 2) then 
            (printFlokemonType "water")
            (removeFlokemonWater)
            (printout t "Successfully deleted!" crlf)
            (printFlokemonType "water")
        )
        (printout t "Press Enter To Continue..")
        (readline)
	)
)

(deffunction findFlokemon ()
    (bind ?find 0)
)

(deffacts inserts
    (flokemonFire (name "Charmeleon") (damage 1200) (defense 250) (level 85) (burnDamage 90) (price 230000))
    (flokemonFire (name "NineTales") (damage 1400) (defense 270) (level 80) (burnDamage 80) (price 540000))
    (flokemonFire (name "Growlithe") (damage 2100) (defense 150) (level 65) (burnDamage 60) (price 300000))
    (flokemonFire (name "Magmar") (damage 400) (defense 120) (level 60) (burnDamage 40) (price 200000))
    (flokemonFire (name "Rapidash") (damage 500) (defense 50) (level 10) (burnDamage 10) (price 50000))
    
    (flokemonWater (name "Blastoise") (damage 1500) (defense 200) (level 80) (price 150000))
    (flokemonWater (name "Goldluck") (damage 2000) (defense 250) (level 90) (price 200000))
    (flokemonWater (name "Poliwhirl") (damage 500) (defense 90) (level 20) (price 250000))
    (flokemonWater (name "Staryu") (damage 800) (defense 70) (level 30) (price 50000))
    (flokemonWater (name "Vaporeon") (damage 900) (defense 120) (level 45) (price 80000))
)

(deffunction mainFlokemon ()
    (reset)
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



