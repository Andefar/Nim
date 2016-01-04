module Model

val initGame : unit -> Async<unit>
val postQ    : 'a -> 'b -> unit