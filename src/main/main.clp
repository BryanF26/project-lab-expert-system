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

(deftemplate matchFlokemon
    (slot name)
    (slot damage)
    (slot defense)
    (slot level)
    (slot burn_damage)
    (slot price)
)

(deftemplate find
    (slot type)
    (slot power)
    (slot defense)
    (slot size)
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
    (format t "|%d.  |%-21s|%-10d|%-10d|%-8d|%-14d|%-9d|" ?*idx* ?n ?dmg ?def ?lvl ?bd ?p)
    (printout t crlf)
    (++ ?*idx*)
)

(defrule print-all-water
    (printWater)
	(flokemonWater (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p))
	=>
    (format t "|%d.  |%-21s|%-10d|%-10d|%-8d|%-9d|" ?*idx* ?n ?dmg ?def ?lvl ?p)
    (printout t crlf)
    (++ ?*idx*)
)

(defrule update-fire
    ;?a adalah variabel inisiasi supaya defrule ini terpanggil
	?a <- (updateFire ?num ?name ?damage ?defense ?level ?burnDamage ?price)
    ?i <- (flokemonFire (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burnDamage ?bd) (price ?p))
	=>
    (if (eq ?num ?*idx*) then
        (modify ?i (name ?name) (damage ?damage) (defense ?defense) (level ?level) (burnDamage ?burnDamage) (price ?price))
    	(retract ?a)
        (bind ?*idx* 1)
    else
        (++ ?*idx*)
    )
)

(defrule update-water
    ;?a adalah variabel inisiasi supaya defrule ini terpanggil
	?a <- (updateWater ?num ?name ?damage ?defense ?level ?price)
    ?i <- (flokemonWater (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p))
	=>
    (if (eq ?num ?*idx*) then
        (modify ?i (name ?name) (damage ?damage) (defense ?defense) (level ?level) (price ?price))
    	(retract ?a)
        (bind ?*idx* 1)
    else
        (++ ?*idx*)
    )
)

(defrule remove-fire
    ?a <- (removeFire ?num)
    ?i <- (flokemonFire (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burnDamage ?bd) (price ?p))
    =>
    (if (eq ?num ?*idx*) then
        (retract ?i)
    	(retract ?a)
        (bind ?*idx* 1)
    else
        (++ ?*idx*)
    )
)

(defrule remove-water
    ?a <- (removeWater ?num)
    ?i <- (flokemonWater (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p))
    =>
    (if (eq ?num ?*idx*) then
        (retract ?i)
    	(retract ?a)
        (bind ?*idx* 1)
    else
        (++ ?*idx*)
    )
)

(defrule find-flokemon
    ?i <- (findFlokemon)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    =>
    (if (eq ?type "Fire") then
    	(assert (findFlokemonFire))
    elif (eq ?type "Water") then
        (assert (findFlokemonWater))
    )
    (retract ?i)
)

(defrule find-flokemon-fire
    ?i <- (findFlokemonFire)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    =>
    (if (eq ?power "Weak") then
    	(assert (findFireWeak))
    elif (eq ?power "Strong") then
        (assert (findFireStrong))
    )
    (retract ?i)
)

(defrule find-fire-weak
    ?i <- (findFireWeak)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    =>
    (if (eq ?defense "Soft") then
    	(assert (findFireWeakSoft))
    elif (eq ?defense "Hard") then
        (assert (findFireWeakHard))
    )
    (retract ?i)
)

(defrule find-fire-weak-soft
    ?i <- (findFireWeakSoft)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    (flokemonFire (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burnDamage ?bd) (price ?p))
    =>
    (if (and (and (and (< ?dmg 1000) (< ?def 100)) (>= ?lvl ?level)) (<= ?p ?budget)) then
    	(assert(matchFlokemon (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burn_damage ?bd) (price ?p)))
    )
    (++ ?*idx*)
    (if (eq ?*idx* ?*totalFire*) then
    	(retract ?i)
        (bind ?*idx* 1)
    )
)

(defrule find-fire-weak-hard
    ?i <- (findFireWeakHard)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    (flokemonFire (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burnDamage ?bd) (price ?p))
    =>
    (if (and (and (and (< ?dmg 1000) (>= ?def 100)) (>= ?lvl ?level)) (<= ?p ?budget)) then
    	(assert(matchFlokemon (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burn_damage ?bd) (price ?p)))
    )
    (++ ?*idx*)
    (if (eq ?*idx* ?*totalFire*) then
    	(retract ?i)
        (bind ?*idx* 1)
    )
)

(defrule find-fire-strong
	?i <- (findFireStrong)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    =>
    (if (eq ?defense "Soft") then
    	(assert (findFireStrongSoft))
    elif (eq ?defense "Hard") then
        (assert (findFireStrongHard))
    )
    (retract ?i)
)

(defrule find-fire-strong-soft
    ?i <- (findFireStrongSoft)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    (flokemonFire (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burnDamage ?bd) (price ?p))
    =>
    (if (and (and (and (>= ?dmg 1000) (< ?def 100)) (>= ?lvl ?level)) (<= ?p ?budget)) then
    	(assert(matchFlokemon (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burn_damage ?bd) (price ?p)))
    )
    (++ ?*idx*)
    (if (eq ?*idx* ?*totalFire*) then
    	(retract ?i)
        (bind ?*idx* 1)
    )
)

(defrule find-fire-strong-hard
    ?i <- (findFireStrongHard)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    (flokemonFire (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burnDamage ?bd) (price ?p))
    =>
    (if (and (and (and (>= ?dmg 1000) (>= ?def 100)) (>= ?lvl ?level)) (<= ?p ?budget)) then
        (assert(matchFlokemon (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (burn_damage ?bd) (price ?p)))
    )
    (++ ?*idx*)
    (if (eq ?*idx* ?*totalFire*) then
    	(retract ?i)
        (bind ?*idx* 1)
    )
)

(defrule find-flokemon-water
    ?i <- (findFlokemonWater)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    =>
    (if (eq ?power "Weak") then
    	(assert (findWaterWeak))
    elif (eq ?power "Strong") then
        (assert (findWaterStrong))
    )
    (retract ?i)
)

(defrule find-water-weak
    ?i <- (findWaterWeak)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    =>
    (if (eq ?defense "Soft") then
    	(assert (findWaterWeakSoft))
    elif (eq ?defense "Hard") then
        (assert (findWaterWeakHard))
    )
    (retract ?i)
)

(defrule find-water-weak-soft
    ?i <- (findWaterWeakSoft)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    (flokemonWater (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p))
    =>
    (if (and (and (and (< ?dmg 1000) (< ?def 100)) (>= ?lvl ?level)) (<= ?p ?budget)) then
    	(assert(matchFlokemon (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p)))
    )
    (++ ?*idx*)
    (if (eq ?*idx* ?*totalWater*) then
    	(retract ?i)
        (bind ?*idx* 1)
    )
)

(defrule find-water-weak-hard
    ?i <- (findWaterWeakHard)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    (flokemonWater (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p))
    =>
    (if (and (and (and (< ?dmg 1000) (>= ?def 100)) (>= ?lvl ?level)) (<= ?p ?budget)) then
    	(assert(matchFlokemon (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p)))
    )
    (++ ?*idx*)
    (if (eq ?*idx* ?*totalWater*) then
    	(retract ?i)
        (bind ?*idx* 1)
    )
)

(defrule find-water-strong
    ?i <- (findWaterStrong)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    =>
    (if (eq ?defense "Soft") then
    	(assert (findWaterStrongSoft))
    elif (eq ?defense "Hard") then
        (assert (findWaterStrongHard))
    )
    (retract ?i)
)

(defrule find-water-strong-soft
    ?i <- (findWaterStrongSoft)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    (flokemonWater (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p))
    =>
    (if (and (and (and (>= ?dmg 1000) (< ?def 100)) (>= ?lvl ?level)) (<= ?p ?budget)) then
    	(assert(matchFlokemon (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p)))
    )
    (++ ?*idx*)
    (if (eq ?*idx* ?*totalWater*) then
    	(retract ?i)
        (bind ?*idx* 1)
    )
)

(defrule find-water-strong-hard
    ?i <- (findWaterStrongHard)
    (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget))
    (flokemonWater (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p))
    =>
    (if (and (and (and (>= ?dmg 1000) (>= ?def 100)) (>= ?lvl ?level)) (<= ?p ?budget)) then
    	(assert(matchFlokemon (name ?n) (damage ?dmg) (defense ?def) (level ?lvl) (price ?p)))
    )
    (++ ?*idx*)
    (if (eq ?*idx* ?*totalWater*) then
    	(retract ?i)
        (bind ?*idx* 1)
    )
)

(defrule delete-match-flokemon
    ?a <- (deleteMatchFlokemon)
    ?i <- (matchFlokemon)
    =>
    (retract ?i)
    (retract ?a)
)

(defrule delete-find
    ?a <- (deleteFind)
    ?i <- (find)
    =>
    (retract ?i)
    (retract ?a)
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
  		(printout t "====================================================================================" crlf)
    	(printout t "|No. |Name                 |Damage    |Defense   |Level   |Burn Damage   |Price    |" crlf)
    	(printout t "====================================================================================" crlf)
    	(assert (printFire))
        (run)
        (retract-string "(printFire)")
    	(printout t "====================================================================================" crlf)  	    
    )
    
    (if (eq ?type "water") then
        (printout t "Water Flokemon List" crlf)
		(printout t "=====================================================================" crlf)
    	(printout t "|No. |Name                 |Damage    |Defense   |Level   |Price    |" crlf)
    	(printout t "=====================================================================" crlf)
    	(assert (printWater))
        (run)
        (retract-string "(printWater)")
    	(printout t "=====================================================================" crlf)		        
    )
    (bind ?*idx* 1)
)

(deffunction viewFlokemon ()
    (bind ?view 0)
   	(while (or (< ?view 1) (> ?view 3))
    	(printout t "Choose Flokemon Type to view" crlf)
		(printout t "1. Fire Flokemon" crlf)
		(printout t "2. Water Flokemon" crlf)
		(printout t "3. Back" crlf)
		(printout t "Choose :" crlf)
    	(bind ?view (read))
        (if (eq (numberp ?view) FALSE) then
        	(bind ?view 0)
        )
    )
    (if(eq ?view 1) then
        (printFlokemonType "fire")
    elif(eq ?view 2) then
        (printFlokemonType "water")
    )
    (printout t "Press Enter To Continue..")
    (readline)
)

(deffunction promptString (?param1 ?param2 ?message)
    (bind ?choice "")
    (while (and (neq ?choice ?param1) (neq ?choice ?param2))
        (printout t ?message)
        (bind ?choice (readline))
        (if (eq (lexemep ?choice) FALSE) then
            (bind ?choice "")
        )
    )
    (return ?choice)
)

(deffunction promptStringWithLength(?lowerBound ?upperBound ?message)
    (bind ?string "")
	(while (or (< (str-length ?string) ?lowerBound) (> (str-length ?string) ?upperBound))
        (printout t ?message)
        (bind ?string (readline))
        (if (eq (lexemep ?string) FALSE) then
            (bind ?string "")
        )
    )
    (return ?string)
)
 
(deffunction promptIntegerWithLength(?lowerBound ?upperBound ?message)
    (bind ?num 0)
	(while (or (< ?num ?lowerBound) (> ?num ?upperBound))
        (printout t ?message)
        (bind ?num (read))
        (if (eq (numberp ?num) FALSE) then
            (bind ?num 0)
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
    (while (or (< ?add 1) (> ?add 3))
    	(printout t "Choose Flokemon Type to add" crlf)
		(printout t "1. Fire Flokemon" crlf)
		(printout t "2. Water Flokemon" crlf)
		(printout t "3. Back" crlf)
		(printout t "Choose :" crlf)
    	(bind ?add (read))
    	(if (eq (numberp ?add) FALSE) then
    		(bind ?add 0)
		)    
    )
    (if(eq ?add 1) then 
    	(addFlokemonFire)
        (printout t "Flokemon successfully added..." crlf)
    elif(eq ?add 2) then 
    	(addFlokemonWater)
 		(printout t "Flokemon successfully added..." crlf)
    )
    (printout t "Press Enter To Continue..")
    (readline)
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
    
    (assert (updateFire ?num ?name ?damage ?defense ?level ?burnDamage ?price))
    
    (run)
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
    
    
    (bind ?name (promptStringWithLength 5 25 "Insert Flokemon Name [5 - 25 Character] : "))

    (bind ?damage (promptIntegerWithLength 10 5000 "Insert Flokemon Damage [10 - 5000] : "))
	
    (bind ?defense (promptIntegerWithLength 5 300 "Insert Flokemon Defense [5 - 300] : "))
	
    (bind ?level (promptIntegerWithLength 1 100 "Insert Flokemon Level [1 - 100] : "))        
    
    (bind ?price (promptIntegerWithLength 1000 1000000 "Insert Flokemon Price [1000 - 1000000] : "))        
    
    (assert (updateWater ?num ?name ?damage ?defense ?level ?price))
    
    (run)
)

(deffunction updateFlokemon ()
    (bind ?update 0)
    (while (or (< ?update 1) (> ?update 3))
    	(printout t "Choose Flokemon Type to update" crlf)
		(printout t "1. Fire Flokemon" crlf)
		(printout t "2. Water Flokemon" crlf)
		(printout t "3. Back" crlf)
		(printout t "Choose :" crlf)
    	(bind ?update (read))
    	(if (eq (numberp ?update) FALSE) then 
        	(bind ?update 0)
        )
    )
    (if(eq ?update 1) then
        (printFlokemonType "fire")
        (updateFlokemonFire)
        (printout t "Successfully update fire flokemon!" crlf)
    elif(eq ?update 2) then 
        (printFlokemonType "water")
        (updateFlokemonWater)
        (printout t "Successfully update water flokemon!" crlf)
    )
    (printout t "Press Enter To Continue..")
    (readline)
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
    (-- ?*totalFire*)
    (assert (removeFire ?num))
    (run)
    
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
    (-- ?*totalWater*)
    (assert (removeWater ?num))
    (run)
    
)

(deffunction removeFlokemon ()
    (bind ?remove 0)
    (while (or (< ?remove 1) (> ?remove 3))
		(printout t "Choose Flokemon Type to removes" crlf)
		(printout t "1. Fire Flokemon" crlf)
		(printout t "2. Water Flokemon" crlf)
		(printout t "3. Back" crlf)
		(printout t "Choose :" crlf)
    	(bind ?remove (read))
    	(if (eq (numberp ?remove) FALSE) then 
        	(bind ?remove 0)
    	)
    )
    (if(eq ?remove 1) then
        (printFlokemonType "fire")
        (removeFlokemonFire)
        (printout t "Successfully deleted!" crlf)
    elif(eq ?remove 2) then 
        (printFlokemonType "water")
        (removeFlokemonWater)
        (printout t "Successfully deleted!" crlf)
    )
    (printout t "Press Enter To Continue..")
    (readline)
)

(deffunction findFlokemon ()
    (printout t "Find Flokemon" crlf)
    (printout t "-----------------" crlf)
    
    (bind ?type (promptString "Fire" "Water" "Demanded type [Fire | Water] : "))
    
    (bind ?power (promptString "Weak" "Strong" "Demanded power [Weak | Strong] : "))
    
    (bind ?defense (promptString "Soft" "Hard" "Demanded defense [Soft | Hard] : "))
    
	(bind ?level (promptIntegerWithLength 1 100 "Demanded minimum flokemon level [1 - 100] : "))
    
    (bind ?budget (promptIntegerWithLength 1000 1000000 "Budget for flokemon [1000 - 1000000] : "))

    (bind ?confirmation (promptString "Y" "N" "Are you sure to find this type of flokemon [Y | N] ? "))
    
    (assert (find (type ?type) (power ?power) (defense ?defense) (size ?level) (price ?budget)))
    
    (if (eq ?confirmation "Y") then
        (bind ?*idx* 0)
        (assert (findFlokemon))
        (run)
    	(new main.GUI)
    )
    
    (assert(deleteMatchFlokemon))
    (assert(deleteFind))
    (run)

    (printout t "Press Enter To Continue..")
    (readline)
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

(defquery retrieve-info
	(find (type ?type) (power ?power) (defense ?defense) (size ?size) (price ?price))
)

(defquery flokemon-found
	(matchFlokemon (name ?name) (damage ?damage) (defense ?defense) (level ?level) (burn_damage ?burn_damage) (price ?price))
)

(deffunction mainFlokemon ()
    (reset)
    (bind ?menus 0)
	(while (neq ?menus 6)
        (run)
    	(clearScreen)
    	(menu)
    	(printout t "Choose : ")
    	(bind ?menus (read))
        (if (eq (numberp ?menus) FALSE) then
            (bind ?menus 0)
        elif (eq ?menus 1) then
            (viewFlokemon)
        elif (eq ?menus 2) then
            (addFlokemon)
        elif (eq ?menus 3) then
            (updateFlokemon)
        elif (eq ?menus 4) then
            (removeFlokemon)
        elif (eq ?menus 5) then
            (findFlokemon)
        else
            (printout t "Thankyou..." crlf)
            (clear)
        )
	)
)

(mainFlokemon)