module Gui


open System
open System.Drawing
open System.Windows.Forms

    // The window part
let window =
  new Form(Text="Game of Nim", Size=Size(525,250))

let input =
  new TextBox(Location=Point(225,25),Size=Size(50,25))

let newGame =
  new Button(Location=Point(200,160),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="Start New Game")

let heap1 =
  new Button(Location=Point(50,65),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="4")

let heap2 =
  new Button(Location=Point(200,65),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="4")

let heap3 =
  new Button(Location=Point(350,65),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="4")

let disable bs = 
    for b in [heap1;heap2;heap2] do 
        b.Enabled  <- true
    for (b:Button) in bs do  
        b.Enabled  <- false

let updateHeap n heap = 
    match heap with
    | '1' -> (heap1.Text <- string(n))
    | '2' -> (heap2.Text <- string(n))
    | '3' -> (heap3.Text <- string(n))
    | _       -> failwith "heap does not exist"

let initGui = 
    window.Controls.Add input
    window.Controls.Add heap1
    window.Controls.Add heap2
    window.Controls.Add heap3
    //This is tests. Eventlisteners must call nim-model functions and not gui functions
//    Application.Run(window)

//
//window.Controls.Add input
//window.Controls.Add heap1
//window.Controls.Add heap2
//window.Controls.Add heap3
//window.Controls.Add newGame
//heap1.Click.Add (fun _ -> updateHeap input.Text '1')
//heap2.Click.Add (fun _ -> updateHeap input.Text '2')
//heap3.Click.Add (fun _ -> updateHeap input.Text '3')
//
//Application.Run(window) 