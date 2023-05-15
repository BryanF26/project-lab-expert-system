(deftemplate orang
	(slot nama)
    (slot umur)
    (slot type)    
)

(deffacts orangs
	(orang (nama "bryan") (umur 10) (type "a"))	
	(orang (nama "felix") (umur 10) (type "a"))	
	(orang (nama "calis") (umur 15) (type "b"))	
)

(defrule muncul
    (defHapus)
	(orang (nama ?nama) (umur ?umur) (type ?type))
    (test (eq ?type "a"))
	=>
    (printout t ?nama " - " ?umur " - " ?type)
)

(reset)
(bind ?num 0)
(while (neq ?num 3)    
   	(printout t "1. View" crlf)
   	(printout t "2. Add" crlf)
   	(printout t "menu : ")
    (bind ?num (read))
    (if (eq ?num 1) then
        (facts)
     elif (eq ?num 2)then 
        (assert (defHapus))
        (run)
        (assert (orang (nama "oasd") (umur 20)))
        (facts)
        (readline)
        (retract-string "(defHapus)"))
)