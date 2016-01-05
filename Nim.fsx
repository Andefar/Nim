#load "Gui.fs"
#load "NimMap.fs"

let h = NimMap.make 12

(*
Authors: Andreas & Silas
Januarkursus - 2015
*)

open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing 

let printCollection msg coll =
        printfn "%s:" msg
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

type heap = string * int
type Message = 
    | Input of heap | Cancel | IllegalInput | Legal | Restart | Hint | Error | Cancelled

let mutable oldMap = NimMap.make 12
let mutable heaps = NimMap.make 12
let mutable first = true
let mutable playersTurn = true
let mutable cancelBool = false
let mutable lastComHeap = ""
let mutable lastComValue = ""
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
          Gui.enable [Gui.heap1;Gui.heap2;Gui.heap3];
          heaps <- NimMap.make 12
          first <- true
          hintsLeft <- 3
          Gui.showHintsLeft ("hint("+(string hintsLeft)+")")
          Gui.disable [Gui.newGame]
          updateGUI heaps
          return! ready()}

and ready() = 
    async{let b = Gui.input.Focus()
          if cancelBool then Gui.printMessage ("You cancelled. Make a match partner ! :=)")
          elif not first && playersTurn then Gui.printMessage ("The computer removed "+lastComValue+" matches from heap "+lastComHeap)
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
          | _              -> return! ready()}

and checkInput h i = 
    async {printfn "%i" i
           first <- false
           playersTurn <- false
           Async.StartImmediate (checkValidInputAndMove h i heaps)
           let! input = eventQ.Receive()
           match input with
           | Legal          -> return! checkStatus()
           | IllegalInput   -> return! ready()
           | _              -> failwith "checkinput failed"}

and checkStatus() = 
    async {updateGUI heaps
           printCollection "" (NimMap.getMap heaps)
           Gui.showInput ""
           Gui.showHint ""
           if (NimMap.win heaps) then return! finished()
           elif playersTurn then
                Gui.enable [Gui.heap1;Gui.heap2;Gui.heap3]
                return! ready()
           else return! computer()
           }

and finished() = 
    async {let mutable winner = "You"
           if playersTurn then winner <- "The computer"
           Gui.printMessage (winner+" WON! :D wanna try again? hit the \"Start New Game\"")
           Gui.enable [Gui.newGame]
           Gui.disable [Gui.heap1;Gui.heap2;Gui.heap3];
           let! input = eventQ.Receive()
           match input with
           | Restart    -> return! initGame()
           | _          -> failwith("finished: You fool")}

and computer() = 
    async {use ts = new CancellationTokenSource()
           
           Gui.disable [Gui.heap1;Gui.heap2;Gui.heap3]
           playersTurn <- true
           printf "so far1"

           Async.StartWithContinuations
               (async{let! input = moveComp heaps
                      return input}, 
               (fun input -> eventQ.Post input),
               (fun _ -> eventQ.Post Error),
               (fun _ -> eventQ.Post Cancelled),
               ts.Token)

           printf "so good"

           let! msg = eventQ.Receive()
           match msg with
           | Legal  -> return! checkStatus() 
           | Error  -> return! initGame()
           | Cancel -> ts.Cancel()
                       return! cancel()
           | _      -> Gui.showHint "Too fast"
                       return! computer()}

and cancel() = 
    async{cancelBool <- true
          heaps <- oldMap
          let! msg = eventQ.Receive()
          match msg with
          | Cancelled | Error -> return! checkStatus()
          | _                 -> failwith "cancel fail"}

let addListeners() = 
    Gui.heap1.Click.Add (fun _ -> eventQ.Post (Input ("1",Gui.text())))
    Gui.heap2.Click.Add (fun _ -> eventQ.Post (Input ("2",Gui.text())))
    Gui.heap3.Click.Add (fun _ -> eventQ.Post (Input ("3",Gui.text())))
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

(*
TODO list: 
    - fix Gui.disable så man ikke kan trykke på knapper selvom man har disabled dem
    - GUI: samle show metoderne til én metode
    - Lave andre extension
    - lav computer ryk til async (push/receive) (l. 187)
*)

