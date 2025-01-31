open SimpleRegexpDef

type 'c reg = 'c SimpleRegexpDef.reg

(** {2 Wyrażenia regularne nad abstrakcyjnym alfabetem}*)


(* Subfunction for simpl checking if two REGEX are IDENTICAL. *)
let rec check_equal r1 r2 = match (r1, r2) with
| (Lit a, Lit b) -> if a = b then true else false
| (Concat (r11, r12), Concat (r21, r22)) -> if check_equal r11 r21 = true && check_equal r12 r22 = true then true else false
| (Or (r11, r12), Or (r21, r22)) -> if check_equal r11 r21 = true && check_equal r12 r22 = true then true else false
| (Star (r11), Star (r22)) -> check_equal r11 r22 
| (Eps, Eps) -> true
| (Empty, Empty) -> true
| _ -> false


(** Upraszczanie wyrażeń regularnych używające prostych własności, 
    oczywiście bez zmiany języka akceptowanych słów.
    Początkowo może to być identyczność, potem można dodać takie uproszczenia jak
      - r | ∅ = r  
      - εr = r
    itp.
*)

let rec simpl regex = match regex with
(* Check for Empty or Epsilon.*)
| Concat (reg1, reg2) -> let ret1 = simpl(reg1) and ret2 = simpl(reg2)
                          in
                          if ret1 = Empty || ret2 = Empty then Empty
                          else if ret1 = Eps then ret2
                          else if ret2 = Eps then ret1
                          else Concat(ret1, ret2)
(* Check for cases when reg1 = reg3 or reg2 = reg3. *)
| Or (Or (reg1, reg2), reg3) -> let ret1 = simpl reg1 and ret2 = simpl reg2 and ret3 = simpl(reg3)
                                in
                                if check_equal ret1 ret3 = true || check_equal ret2 ret3 = true then simpl (Or (ret1, ret2))
                                else Or (simpl (Or (reg1, reg2)), ret3)
(* Check for reg1 = reg2, or Empty. *)
| Or (reg1, reg2) -> let ret1 = simpl(reg1) and ret2 = simpl(reg2)
                      in
                      if check_equal ret1 ret2 = true then ret1
                      else if ret1 = Empty then ret2
                      else if ret2 = Empty then ret1
                      else Or (ret1, ret2)
(* Check for Empty or Epsilon. *)
| Star (reg) -> let ret = simpl(reg)
                  in
                  if ret = Empty then Empty
                  else if ret = Eps then Eps
                  else Star(ret)
| _ -> regex;;


(** Czy ε należy do języka? *)
let rec nullable regex = match regex with
| Lit(_) -> false
| Concat (reg1, reg2) -> if nullable reg1 = true && nullable reg2 = true then true else false
| Or (reg1, reg2) -> if nullable reg1 = true || nullable reg2 = true then true else false
| Star(_) -> true
| Eps -> true
| Empty -> false;;


(** Czy akceptowany język jest pusty? *) 
let rec empty regex = match regex with
| Lit (_) -> false
| Concat (reg1, reg2) -> if empty reg1 = true || empty reg2 = true then true else false
| Or (reg1, reg2) -> if empty reg1 = true && empty reg2 = true then true else false
| Star (_) -> false
| Empty -> true
| Eps -> false;;


(** [der a r] to pochodna [r] względem [a], czyli wyrażenie regularne [r'], 
    które akceptuje takie słowa [w], że słowo [aw] jest akceptowane przez [r].
    Np. pochodna wyrażenia ab* po a to oczywiście b*, pochodna a* po a to a*, 
    a pochodna b po a to ∅ (wyrażenie nieakceptujące żadnego słowa). 
    Jaka jest pochodna a*bc po b ? A jaka a*ac po a ? A jaka (a*a|ab)c po a ? *)

(* All outputs are simplified. *)
let rec der a r = match r with
(* Check for x = a *)
| Lit(x) -> if x = a then Eps else Empty
(* If first part of Concat is nullable then output is an Or, else output derived concat.*)
| Concat (reg1, reg2) -> let ret1 = der a reg1 and ret2 = der a reg2 
                          in
                          if nullable reg1 then simpl(Or (Concat(ret1, reg2), ret2)) else simpl(Concat(ret1, reg2))
(* Simplify derived Or. *)
| Or (reg1, reg2) -> let ret1 = der a reg1 and ret2 = der a reg2 
                        in
                        simpl (Or (ret1, ret2))
(* Concatenation of derived value and star, because star can be used multiple times. *)
| Star(x) -> simpl(Concat(der a x, Star(x)))
| _ -> Empty;;        


(** [ders v r] to pochodna [r] względem [v], czyli wyrażenie regularne [r'], 
    które akceptuje takie słowa [w], że słowo [vw] jest akceptowane przez [r].
    W implementacji należy użyć [der], niewątpliwie przyda się też [simpl]. *)

(* Use der for every element of els. *)
let rec ders els regex = match els with
| h :: t -> ders (t) (der h regex)
| _ -> regex;;

(** Czy słowo jest akceptowane przez wyrażenie regularne?
    W implementacji należy użyć [ders] i [nullable]. *)

(* Check if REGEX is nullable after getting necessary part.*)
let accept regex els = nullable (ders els regex);;


(** Prezentacja wyrażenia regularnego w postaci napisu. *)
let repr conv regex = 
  (* less_brackets helps with deleting redundant brackets around Or. *)
  let rec less_brackets reg is_or = match reg with
    | Lit(l) -> conv(l)
    | Concat(reg1, reg2) ->  less_brackets reg1 false ^ less_brackets reg2 false
    | Or(reg1, reg2) -> if is_or = true then less_brackets reg1 true ^ "|" ^ less_brackets reg2 true
                        else "(" ^ less_brackets reg1 true ^ "|" ^ less_brackets reg2 true  ^ ")"
    | Star(reg1) -> "(" ^ less_brackets reg1 true ^ ")*"
    | Eps -> "ε"
    | Empty -> "∅"
  in
  less_brackets regex false;; 


(** {2 Użyteczne funkcje dla [char reg]} *)

(** Wyrażenie regularne akceptujące język złożony z jednego jednoliterowego słowa. *)

let char ch = match ch with
| '\000' -> Eps
| _ -> Lit(ch);;


(** Wyrażenie regularne akceptujące język złożony z jednego słowa. *)

let string str = 
  let length = String.length str 
  in
  if length = 0 then Eps 
  (* Creating nested concats of Lit of every char. *)
  else String.fold_right(fun c e -> Concat(Lit(c), e)) (String.sub str 0 (length - 1)) (Lit(str.[length - 1]));;


(** Wyrażenie regularne akceptujace język złożony z jednoliterowych słów 
    dla wszystkich liter z podanego napisu. *)

let alts str = 
  let length = String.length str 
  in
  if length = 0 then Eps 
  (* Creating nested ors of Lit of every char. *)
  else String.fold_right(fun c e -> Or(Lit(c), e)) (String.sub str 0 (length - 1)) (Lit(str.[length - 1]));;


(** Specjalizacja accept dla [char reg] i [string]. *)
let accepts regex s = 
  (* Changing string to list. *)
  let list = String.fold_right (fun c acc -> c :: acc) s [] 
  in 
  accept regex list;;


(** Zamiana [char reg] na napis. Proszę postarać się o niewstawianie niepotrzebnych nawiasów. *)
let to_string regex = 
  (* less_brackets helps with deleting redundant brackets around Or. *)
  let rec less_brackets reg is_or = match reg with
    | Lit(l) -> String.make 1 l
    | Concat(reg1, reg2) ->  less_brackets reg1 false ^ less_brackets reg2 false
    | Or(reg1, reg2) -> if is_or = true then less_brackets reg1 true ^ "|" ^ less_brackets reg2 true
                        else "(" ^ less_brackets reg1 true ^ "|" ^ less_brackets reg2 true  ^ ")"
    | Star(reg1) -> "(" ^ less_brackets reg1 true ^ ")*"
    | Eps -> "ε"
    | Empty -> "∅"
  in
  less_brackets regex false;; 

(** Zamiana napisu na wyrażenie regularne. Proszę zajrzeć do {!README.md}. *)
let parse s = Parser.regex Lexer.token (Lexing.from_string s)
