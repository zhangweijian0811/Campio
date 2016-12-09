(*
 * Sharpstone: a tiny card game simulator
 *
 * Written as project exam template for Computer Science, Laboratorio di Programmazione
 * Freely adapted from Heartstone (TM) by Blizzard Entertainment, Inc.
 *
 * (C) 2016 Alvise Spanò @ DAIS, Università Ca' Foscari, Venezia
 *)

module LabProg2016.Sharpstone

#if INTERACTIVE
#r "System.Runtime.Serialization.dll"
#endif

open System
open System.IO
open System.Runtime.Serialization
open System.Text

// globals
//

let rand = new Random ()    // remove seed argument for making randomness indeterministic

/// Generate a random integer within the interval (a, b) inclusively.
let rnd_int a b = rand.Next (a, b + 1) 


// type definitions
//

/// Defines the card type.
[< DataContract; StructuralEquality; NoComparison >]
type card = {
    [< field: DataMember(Name = "id") >] id : string
    [< field: DataMember(Name = "name") >] name : string
    [< field: DataMember(Name = "cost") >] cost : int
    [< field: DataMember(Name = "type") >] typee : string
    [< field: DataMember(Name = "attack") >] attack : int
    [< field: DataMember(Name = "health") >] mutable health : int
}
with
    override c.ToString () = sprintf "%s [Id:%s  Atk:%d  HP:%d]" c.name c.id c.attack c.health

/// Deck type alias.
type deck = card list

/// Defined the player type.
[< StructuralEquality; NoComparison >]
type player = {
    name : string
    mutable life : int
    mutable deck : deck
}
with
    override p.ToString () = sprintf "%s [Life:%d  Deck:%d]" p.name p.life p.deck.Length


// JSON stuff
//

/// Convert a JSON string into a typed value.
let unjson<'t> (input : string) : 't =  
    use ms = new MemoryStream(ASCIIEncoding.Default.GetBytes(input)) 
    let obj = (new Json.DataContractJsonSerializer(typeof<'t>)).ReadObject(ms) 
    obj :?> 't

/// Parse a JSON deck given the filename.
let parse_deck (filename : string) = 
    use fstr = File.OpenRead filename
    use rd = new StreamReader (fstr)
    printfn "Parsing JSON file \"%s\"..." fstr.Name
    rd.ReadToEnd () |> unjson<card[]> |> Array.toList


// printers
//

/// Prints the turn number header. Call this function at the beginning of each turn.
let print_turn_begin (turn : int) = printfn "\nTurn %d:" turn

/// Prints the status of the 2 players. Call this function at the end of each turn.
let print_turn_end (p1 : player, p2 : player) = printfn "\t%O\n\t%O" p1 p2

/// Prints the information of 2 cards fighting. Call this function at each turn when both players have a card.
let print_turn_2cards (c1 : card, c2 : card) = printfn "%O VS %O" c1 c2

/// Prints the information of 1 card fighting against a player with no cards. Call this function at each turn when only 1 players have a card.
let print_turn_1card (p : player, c : card) = printfn "* %O VS player %O" c p

/// Prints the information of 2 players when both have no cards. Call this function at each turn no cards have been drawn.
let print_turn_no_cards (p1 : player, p2 : player) = printfn "* Both %O and %O have no cards" p1 p2

/// Prints the information of a dead cards. Call this function when a card dies.
let print_card_death (c : card) = printfn "+ %O died (%d overkill)" { c with health = 0 } -c.health



// combat mechanics
//
let rec togliere_morto (deck : card list)(card : card): card list =
    match deck with
    []->[]
    |[x]-> if x = card then []
                       else [x]
    |x::xs -> if x = card then xs
                          else  x :: togliere_morto xs card 

// fun filter_deck, dato un mazzo restituisce solo le carte Minion
let rec filter_deck (deck : card list) = 
    match deck with
    [] -> []
    |x::xs -> if x.typee = "MINION" && x.attack > 0 && x.health > 0 then x::(filter_deck xs)
              else filter_deck(xs)

//empty card è una carta del mazzo che non ha valore
let empty_card = {id="nulla"; name = "carta nulla" ; cost=0 ; typee="nulla"; attack=0 ; health=0}

let non_mana = {id="nulla"; name = "carta nulla" ; cost=0 ; typee="niente"; attack=0 ; health=0}
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
    |[x]-> if (choose_card (pesca_carta_migliore deck)) = choose_card x 
           then [x]
           else []
    |x::y::xs -> if choose_card (pesca_carta_migliore deck ) = choose_card x
                 then (x)::pesca_carta_uguale(y::xs)
                 else  pesca_carta_uguale(y::xs)

let rec quante_carte_uguale (deck : card list) : int =
    match deck with 
    [] -> 0
    |[x] -> 1
    |x::xs -> (1 + quante_carte_uguale xs )

//n  =  rnd_int 1 ((quante_carte_uguale (pesca_carta_uguale p1.deck))

let rec casuale_carta (deck : card list)(n : int) : card = // casuale_carta (quante_carte_uguale (pesca_carta_uguale p1.deck)) (quante_carte_uguale (pesca_carta_uguale p1.deck))
    match deck with 
    []-> non_mana
    |[x]->x
    |x::xs -> if n=1 then x
              else casuale_carta xs (n-1)
    
//fun pesca_carta_definitiva, dato un mazzo e il mana, restituisce la carta migliore sia in termini di mana che in termini di attack/health
let rec pesca_carta_definitiva (deck : card list) (mana : int) : card =
    match deck with
    [] -> empty_card
    |[x] -> if x.cost <= mana then x else empty_card
    |x::xs -> if (casuale_carta (pesca_carta_uguale (pesca_carta_uguale deck)) (rnd_int 1 (quante_carte_uguale (pesca_carta_uguale deck)))).cost <= mana 
              then casuale_carta (pesca_carta_uguale (pesca_carta_uguale deck)) (rnd_int 1 ((quante_carte_uguale (pesca_carta_uguale deck))))
              else non_mana

//Combattimento 
let fight (deck1 : deck) (deck2 : deck) : player * player * int =
    let p1 = { name ="P1"; life = 30; deck = filter_deck deck1 }    
    let p2 = { name ="P2"; life = 30; deck = filter_deck deck2 }

    let mutable turn = 1 //variabile contatore del turno
    let mutable quit = false //flag per uscita d'emergenza dal ciclo
    while not quit && p1.life > 0 && p2.life > 0 do
          print_turn_begin turn //stampa inizio del turno
          let mana = if turn > 10 then 10 else turn  //inizio estrazione carte
          let c1 = pesca_carta_definitiva p1.deck mana  //scelta carta 1
          let c2 = pesca_carta_definitiva p2.deck mana  //scelta carta 2

          match c1, c2 with

            |c1,c2 when c1= empty_card && c2 = empty_card -> print_turn_no_cards (p1,p2) //p1 e p2 hanno finito le carte
                                                             quit<-true
            |c1,c2 when c1= non_mana && c2= non_mana -> print_turn_no_cards (p1,p2)      
                 
            |c1,c2 when c1.typee="niente" && c2.typee="MINION" -> print_turn_1card (p1,c2) 
                                                                  p1.life <- p1.life-c2.attack //c1 non ha niente, c2 ha la carta
            |c1,c2 when c2.typee="MINION" && c2.typee="niente" -> print_turn_1card (p2,c1) 
                                                                  p2.life <- p2.life-c1.attack //c1 ha la carta, c2 non ha niente            
            |_,_-> if c1.attack > c2.health && c2.attack > c1.health && c1.typee="MINION" && c2.typee="MINION" //se il attack di c1 grande di il health di c2 e il attack di c2 grande di il health di c1
                      then print_turn_2cards (c1,c2) 
                           p1.life <- p1.life - (c2.attack - c1.health) 
                           p2.life <- p2.life - (c1.attack - c2.health)
                           print_card_death c1
                           print_card_death c2
                           p1.deck<- togliere_morto p1.deck c1
                           p2.deck<- togliere_morto p2.deck c2

                      elif c1.attack > c2.health && c2.attack = c1.health && c1.typee="MINION" && c2.typee="MINION"// se il attack di c1 grande di il health di c2 e il attack di c2 piccolo di il health di c1
                        then print_turn_2cards (c1,c2) 
                             p1.life<- p1.life
                             p2.life<- p2.life - (c1.attack - c2.health)
                             print_card_death c1
                             print_card_death c2
                             p1.deck<- togliere_morto p1.deck c1
                             p2.deck<- togliere_morto p2.deck c2

                      elif c1.attack > c2.health && c2.attack < c1.health && c1.typee="MINION" && c2.typee="MINION"// se il attack di c1 grande di il health di c2 e il attack di c2 piccolo di il health di c1
                        then print_turn_2cards (c1,c2) 
                             p1.life<- p1.life
                             p2.life<- p2.life - (c1.attack - c2.health)
                             print_card_death c2
                             p2.deck<- togliere_morto p2.deck c2

                      elif c1.attack < c2.health && c2.attack > c1.health && c1.typee="MINION" && c2.typee="MINION" // se il attack di c1 piccolo di il health di c2 e il attack di c2 grande di il health di c1
                         then print_turn_2cards (c1,c2) 
                              p1.life<- p1.life - (c2.attack - c1.health)
                              p2.life<- p2.life
                              print_card_death c1
                              p1.deck<- togliere_morto p1.deck c1
                      
                      elif c1.attack < c2.health && c2.attack = c1.health && c1.typee="MINION" && c2.typee="MINION" // se il attack di c1 piccolo di il health di c2 e il attack di c2 grande di il health di c1
                         then print_turn_2cards (c1,c2) 
                              p1.life<- p1.life - (c2.attack - c1.health)
                              p2.life<- p2.life
                              print_card_death c1
                              p1.deck<- togliere_morto p1.deck c1
                 
                     elif c1.attack < c2.health && c2.attack < c1.health && c1.typee="MINION" && c2.typee="MINION" // se loro devono attacarsi molte volte
                         then print_turn_2cards (c1,c2) 
                              p1.life<- p1.life
                              p2.life<- p2.life
                              c1.health<-c1.health-c2.attack
                              c2.health<-c2.health-c1.attack  

                     elif c1.attack = c2.health && c2.attack > c1.health && c1.typee="MINION" && c2.typee="MINION" // se il attack di c1 piccolo di il health di c2 e il attack di c2 grande di il health di c1
                         then print_turn_2cards (c1,c2) 
                              p1.life<- p1.life - (c2.attack - c1.health)
                              p2.life<- p2.life 
                              print_card_death c1
                              p1.deck<- togliere_morto p1.deck c1

                     elif c1.attack = c2.health && c2.attack < c1.health && c1.typee="MINION" && c2.typee="MINION" // se il attack di c1 piccolo di il health di c2 e il attack di c2 grande di il health di c1
                         then print_turn_2cards (c1,c2) 
                              p1.life<- p1.life
                              p2.life<- p2.life
                              print_card_death c2
                              p1.deck<- togliere_morto p1.deck c2
                                            
                     elif c1.attack = c2.health && c2.attack = c1.health && c1.typee="MINION" && c2.typee="MINION" // se loro devono attacarsi molte volte
                         then print_turn_2cards (c1,c2) 
                              p1.life<- p1.life
                              p2.life<- p2.life
                              print_card_death c1
                              print_card_death c2
                              p1.deck<- togliere_morto p1.deck c1
                              p2.deck<- togliere_morto p2.deck c2           
                                             

          print_turn_end (p1,p2)
          turn <- turn + 1
    
       // a partita conclusa stampiamo l'esito
    if p1.life = p2.life then printfn "Tie"
    elif p1.life > p2.life then printfn "P1 wins"
    else printfn "P2 wins"
    // e ritorniamo lo stato dei due player e l'ultimo turno giocato
    p1, p2, turn - 1


// main code
//

[< EntryPoint >]
let main argv =
    let code =
        try
            if argv.Length <> 2 then
                printfn "Usage: Sharpstone <DECK1> <DECK2>"
                0
            else
                let p filename = parse_deck filename    // function for parsing a JSON file defining a deck as a list of cards
                let d1 = p argv.[0]                     // parse the first argument of the executable (DECK1)
                let d2 = p argv.[1]                     // parse the second argument of the executable (DECK2)
                let p1, p2, turn as r = fight d1 d2
                // print final result
                printfn "\nResult:\n\t%d Turns\n\t%O\n\t%O\n\tHash: %X" turn p1 p2 (r.GetHashCode ())
                0

        with e -> printfn "Uncaught exception: %O" e; 1

    #if DEBUG
    printfn "\n\nPress any key to exit..."
    Console.ReadKey () |> ignore
    #endif
    code
