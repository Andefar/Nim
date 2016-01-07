#load "Gui.fs"
#load "NimMap.fs"

(*
Authors: Andreas & Silas
Januarkursus - 2015
*)

open System 
open System.Net 
open System.Threading  
open System.Windows.Forms 


let printCollection msg coll =
        Seq.iteri (fun index item -> printfn "  %i: %O" index item) coll
       
// An asynchronous event queue kindly provided by Don Syme 
type AsyncEventQueue<'T>() = 
    let mutable cont = None 
    let queue = System.Collections.Generic.Queue<'T>()
    let tryTrigger() = 
        match queue.Count, cont with 
        | _, None -> ()
        | 0, _ -> ()
        | _, Some d -> 
            cont <- None
            d (queue.Dequeue())

    let tryListen(d) = 
        if cont.IsSome then invalidOp "multicast not allowed"
        cont <- Some d
        tryTrigger()

    member x.Post msg = queue.Enqueue msg; tryTrigger()
    member x.Receive() = 
        Async.FromContinuations (fun (cont,econt,ccont) -> 
            tryListen cont)

// Automaton
type Message = | Input of string * int | Cancel 
               | IllegalInput | Legal | Restart | Hint | Error | ThreadCancelled

let mutable oldMap = NimMap.make 12
let mutable heaps = NimMap.make 12
let mutable first = true
let mutable playersTurn = true
let mutable cancelBool = false
let mutable illegalAction = false
let mutable lastComHeap = ""
let mutable lastComValue = ""
let mutable lastPlayerHeap = ""
let mutable lastPlayerValue = 0
let mutable hintsLeft = 3

let eventQ = AsyncEventQueue()

let checkValidInputAndMove h i map  = 
    async{let curMatches = (NimMap.find h map) - i

          if (curMatches >= 0 && i > 0) then
            oldMap <- map
            heaps <- NimMap.update h curMatches heaps
            eventQ.Post Legal
          else
            Gui.printMessage "Input is invalid :P go again.."
            eventQ.Post IllegalInput}

let moveComp map = 
    async{do! Async.Sleep(3000)
          let optimal = NimMap.calcOptimal map
          oldMap <- map
          heaps <- NimMap.update (fst optimal) (snd optimal) map
          let diff = (NimMap.find (fst optimal) map) - (snd optimal)
          lastComValue <- string (diff)
          lastComHeap <- (fst optimal)
          return Legal}

let updateGUI map = Map.iter (fun key value -> Gui.updateHeap key value) (NimMap.getMap map)
                    

let rec initGame() =
    async{Gui.printMessage "Welcome! Enter number of matches you want to remove and select a heap"
          playersTurn <- true
          heaps <- NimMap.make 12
          first <- true
          hintsLeft <- 3
          Gui.showHintsLeft ("hint("+(string hintsLeft)+")")
          Gui.disable [Gui.newGame]
          updateGUI heaps
          return! ready()}

and ready() = 
    async{let b = Gui.input.Focus()
          Gui.disable []
          
          if not cancelBool && not first && playersTurn then 
                Gui.printMessage ("The computer removed "+lastComValue+" matches from heap "+lastComHeap)
          
          cancelBool <- false
          
          let! input = eventQ.Receive()
          match input with
          | Input (h,i)    -> return! checkInput h i 
          | Hint           -> let opt = NimMap.calcOptimal heaps
                              let diff = string ((NimMap.find (fst opt) heaps) - (snd opt))
                              if hintsLeft > 0 then Gui.showHint ("h"+(string (fst opt)+" m"+ diff))
                              else Gui.showHint "LOL"
                              if hintsLeft>0 then hintsLeft <- hintsLeft-1
                              Gui.showHintsLeft ("hint("+(string hintsLeft)+")")
                              return! ready()
          | Restart        -> return! initGame()
          | Cancel         -> cancelBool <- true
                              Gui.printMessage ("Nothing to cancel")
                              return! ready()
          | _              -> return! ready()}

and checkInput h i = 
    async {first <- false
           playersTurn <- false
           Async.StartImmediate (checkValidInputAndMove h i heaps)
           let! input = eventQ.Receive()
           match input with
           | Legal          -> lastPlayerHeap <- h
                               lastPlayerValue <- i
                               return! checkStatus()
           | IllegalInput   -> return! ready()
           | _              -> failwith "checkinput failed"}

and checkStatus() = 
    async {updateGUI heaps
           Gui.showInput ""
           Gui.showHint ""
           if (NimMap.win heaps) then 
                if playersTurn then return! finished("The computer WON! :D You should try again? Hit the \"Start New Game\"")
                else                return! finished("You WON! :D Wanna try again? Hit the \"Start New Game\"")
           elif playersTurn then
                return! ready()
           else return! computer()}

and computer() = 
    async {Gui.printMessage "Computer is thinking, please wait..."
           use ts = new CancellationTokenSource()
           
           Gui.disable Gui.heaps

           // code below is matching when the thread is interrupted by actions other than the Cancel Button
           if (cancelBool) then
             let! msg = eventQ.Receive()
             match msg with
             | ThreadCancelled -> Gui.printMessage "Too fast.. you are interrupting the computer :D "
                                  cancelBool <- false
             | _               -> failwith "computer: this should not happen"
           
           Async.StartWithContinuations
                (async{let! input = moveComp heaps 
                return input}, 
                (fun input -> eventQ.Post input),
                (fun _ -> eventQ.Post Error),
                (fun _ -> cancelBool <- true
                          eventQ.Post ThreadCancelled),
                ts.Token)

           let! msg = eventQ.Receive()
           match msg with
           | Legal  -> playersTurn <- true
                       return! checkStatus() 
           | Error  -> eventQ.Post Restart
                       return! finished("An error accured in computer")
           | Cancel -> ts.Cancel()
                       playersTurn <- true
                       return! cancel()
           | _      -> ts.Cancel()
                       return! computer()}

and cancel() = 
    async{heaps <- oldMap
          let! msg = eventQ.Receive()
          match msg with
          | ThreadCancelled | Error -> Gui.printMessage ("You cancelled. Make a match partner ! :D")
                                       return! checkStatus()
          | _                 -> failwith ("cancel fail" + string(msg))}

and finished(s) = 
    async {Gui.printMessage (string(s))
           Gui.enable [Gui.newGame]
           Gui.disable (Gui.heaps@[Gui.cancel;Gui.hintBut]);
           let! input = eventQ.Receive()
           match input with
           | Restart    -> return! initGame()
           | _          -> failwith("finished: You fool")}

let addListeners() = 
    for x in Gui.heaps do
        x.Click.Add (fun _ -> eventQ.Post (Input (string((List.findIndex (fun elem -> elem = x) Gui.heaps) + 1),Gui.text())))

    Gui.cancel.Click.Add (fun _ -> eventQ.Post Cancel)
    Gui.newGame.Click.Add (fun _ -> eventQ.Post Restart)
    Gui.hintBut.Click.Add (fun _ -> eventQ.Post Hint)

let startGame() =
    Gui.initGui()
    addListeners()
    Async.StartImmediate (initGame())
    Gui.window.Show()
//    Application.Run(Gui.window)
 
startGame()
