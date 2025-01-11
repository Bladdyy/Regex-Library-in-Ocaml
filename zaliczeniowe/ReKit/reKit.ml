(** Typ wyrażeń regularnych *)
type t = Regextkit.Tree.re;;

(** Parser powinien być w stanie sparsować wyrażenia regularne używające 
  składni obowiązującej w zadaniu. *)
let re str = Regextkit.Re.parse (String.map (fun c -> if c = '|' then '+' else c) str);; (* Changing '|' to '+' then parsing.*)

(** Drukowanie reprezentacji wyrażeń regularnych na stdout; 
  na końcu powinno być przejście do nowego wiersza. *)
let debug regex = Regextkit.Re.print regex;;

(** Sprawdzanie czy {b cały} napis pasuje do wyrażenia regularnego. *)
let matches regex str = 
  let lens = String.length str 
  in
  let rec der reg ind = (* Derivating char by char *)
    if ind = lens then reg
    else der (Regextkit.Re.simplify(Regextkit.Re.derivative reg (String.make 1 str.[ind]))) (ind + 1)
  in 
  Regextkit.Re.is_nullable (der regex 0);; (* Check if end regex nullable. *)

