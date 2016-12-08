// fun filter_deck, dato un mazzo restituisce solo le carte Minion
let rec filter_deck (deck : card list) = 
    match deck with
    [] -> []
    |x::xs -> if x.typee = "MINION" && x.attack > 0 then x::(filter_deck xs)
              else filter_deck(xs)

//empty card è una carta del mazzo che non ha valore
let empty_card = {id="nulla"; name = "carta nulla" ; cost=0 ; typee="nulla"; attack=0 ; health=0}

let niente_card = {id="niete"; name = "carta vuota" ; cost=0 ; typee="niete"; attack=0 ; health=0}
//fun pesca_carta_mana, pesca una carta del mazzo se il costo non è maggiore del mana
let rec pesca_carta_mana (deck : card list) (mana : int) : card = 
    match deck with
    [] -> empty_card
    |[x] -> if x.cost <= mana then x else empty_card
    |x::xs -> if x.cost > mana then pesca_carta_mana xs mana
                 else x
//scegli la carta con il rapporto tra attacco e salute , p = card.attack/card.health
let choose_card (card : card) = card.attack/card.health

//fun pesca_carta_migliore, dato un mazzo prende la carta migliore 
let rec pesca_carta_migliore (deck : card list) : card =
    match deck with
    [] -> empty_card
    |[x] -> x
    |x::y::xs -> if choose_card x < choose_card y
                 then pesca_carta_migliore(y::xs)
                 else pesca_carta_migliore(x::xs)

// una lista che contiene carte uguali p
let rec pesca_carta_uguale (deck : card list) : card list =
    match deck with
    []->[]
    |[x]-> if choose_card (pesca_carta_migliore deck ) = choose_card x 
           then []
           else [x]
    |x::y::xs -> if choose_card (pesca_carta_migliore deck ) = choose_card x
                 then (x)::pesca_carta_uguale(y::xs)
                 else  pesca_carta_uguale(y::xs)

let rec quante_carte_uguali (deck : card list) : int =
    match deck with 
    [] -> 0
    |[x] -> 1
    |x::xs -> (1 + quante_carte_uguali xs )

let rec casuale_carta (deck : card list)(n : int) : card = // casuale_carta (quante_carte_uguali (pesca_carta_uguale p1.deck)) (quante_carte_uguali (pesca_carta_uguale p1.deck))
    match deck with 
    []-> niente_card
    |[x]->x
    |x::xs -> if n=1 then x
              else casuale_carta xs (n-1)

//io non cambio
//io non cambio
//io non cambio
//fun pesca_carta_definitiva, dato un mazzo e il mana, restituisce la carta migliore sia in termini di mana che in termini di attack/health
let rec pesca_carta_definitiva (deck : card list) (mana : int) : card =
    match deck with
    [] -> empty_card
    |[x] -> if x.cost <= mana then x else empty_card
    |x::xs -> if (casuale_carta (pesca_carta_uguale deck)).cost <= mana then casuale_carta (pesca_carta_uguale deck)
              else niente_card
