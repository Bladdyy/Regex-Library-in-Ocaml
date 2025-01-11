(** Typ wyrażeń regularnych *)
type t = Regextkit.Tree.re;;

(** Parser powinien być w stanie sparsować wyrażenia regularne używające 
  składni obowiązującej w zadaniu. *)
let re str = Regextkit.Re.parse str;;

(** Drukowanie reprezentacji wyrażeń regularnych na stdout; 
  na końcu powinno być przejście do nowego wiersza. *)
let debug regex = Regextkit.Re.print regex;;

(** Sprawdzanie czy {b cały} napis pasuje do wyrażenia regularnego. *)
let matches regex str = 
  let rec der reg s = 
    let len = String.length s
    in
    if len = 0 then reg
    else der (Regextkit.Re.derivative (reg) (String.make 1 s.[0])) (String.sub str 1 (len - 1))
  in 
  Regextkit.Re.is_nullable (der regex str);;

