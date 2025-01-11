(** Typ wyrażeń regularnych *)
type t = char SimpleRegexp.reg

(** Parser powinien być w stanie sparsować wyrażenia regularne używające
  składni obowiązującej w zadaniu. *)
let re str = SimpleRegexp.parse str

(** Drukowanie reprezentacji wyrażeń regularnych na stdout;
  na końcu powinno być przejście do nowego wiersza. *)
let debug regex = Printf.printf "Regex: %s\n" (SimpleRegexp.to_string regex)

(** Sprawdzanie czy {b cały} napis pasuje do wyrażenia regularnego. *)
let matches regex str = SimpleRegexp.accepts regex str
