open Regexp

module Test(Re : REGEXP) = struct

    (** {2 Elementy pomocnicze} *)
    
    (** Język to zbiór słów. Użyj modułu [Set] lub podobnego. *)
    module StringSet = Set.Make(String)
    type lang = StringSet.t

    (** Tworzenie losowego zbioru słów z podanych liter. Do losowania należy 
        używać modułu [Random], parametr liczbowy powinien jakoś określać rozmiar 
        zbioru (im większa liczba tym większy zbiór). Może to być po prostu 
        liczba elementów zbioru, ale niekoniecznie. *)
    let make_lang str num =
      let rec gen_word todo acc =
        if todo <= 0 then acc
        else 
          let len = Random.int (num) + 1 (* Words of length equal to max num + 1 *)
          and strlen = String.length str
          in
          let word = String.init len (fun _ -> str.[Random.int (strlen)])  (* Generate word *)
          in
          let set = StringSet.add word acc
          in
          if StringSet.cardinal set = StringSet.cardinal acc then gen_word todo acc  (* If created duplicate *)
          else gen_word (todo - 1) (set)
      in
      gen_word num StringSet.empty


      
    (** Wybieranie z języka tych słów, które są akceptowane przez dane 
        wyrażenie regularne. *)
    let select_accepted regex lang =
      StringSet.filter (fun word -> Re.matches regex word) lang


    (** Porównanie dwóch wyrażeń regularnych z użyciem podanego języka.
        Procedura powinna wybierać z języka słowa akceptowane przez każde 
        z wyrażeń regularnych za pomocą [select_accepted], a
        następnie porównywać uzyskane zbiory za pomocą [eq] z modułu [Set].
        Wynikiem powinna być liczba błędów (dowolnie liczona, byle dodatnia 
        jeśli wynik jest nieprawidłowy) oraz czas łącznego wykonania obu wywołań
        [select_accepted]. Do liczenia czasu można używać [Sys.time]. 
        Ostatni parametr boolowski może służyć zwiększeniu poziomu wypisywania 
        szczegółów działania (debug on/off). *)
    let test_two regex1 regex2 lang debug =
      let start_time = Sys.time() 
      in
      let lang1 = select_accepted regex1 lang 
      and lang2 = select_accepted regex2 lang
      in
      let end_time = Sys.time()
      in
      let error = if StringSet.equal lang1 lang2 then 0 else 1
      in
      if debug then (
        Printf.printf "Debug for:\n";
        Re.debug regex1;
        Re.debug regex2;
        Printf.printf "Is there an error (1 - there is, 0 - there is not): %d\n\n" error;
      );
      (error, end_time -. start_time)


    (** Najważniejsza funkcja w funktorze testującym. Uruchamia wszystkie testy 
        (patrz niżej), w wyniku daje łączną liczbę błędów i czas działania, 
        w miarę możliwości ograniczony do wywołań funkcji na wyrażeniach 
        regularnych (bez np. konstrukcji zbioru). *)
    let test () =
      let errors = ref 0 
      and total_time = ref 0.0
      in
  
      (* 1. Złośliwe testy na pojedynczych napisach, np.
      - Czy ba*b akceptuje baabaabaaab i baaaaaaab ?
      - Czy a*b akceptuje aaaaaaa i aaaaab ? 
        (tu można użyć naprawdę długich napisów, np. takich na 1000000 znaków)*)

      let regex1 = Re.re "ba*b" 
      and regex2 = Re.re "a*b" 
      and long_a = String.make 1000000 'a'
      in
      let mals1 = [ (* Words to test first language. *)
        ("baabaabaaab", false);
        ("baaaaaaab", true);
        ("b" ^ long_a ^ "b", true);
        ("b" ^ long_a ^ String.make 10000 'b' ^ "aab", false)        
      ]
      and mals2 = [ (* Words to test second language. *)
        ("aaaaaaa", false);
        ("aaaaab", true);
        (long_a ^ "b", true)
      ]
      in
      (* Defining tests for word inclusion. *)
      let iter_tests regex words debug = 

        if debug then (
          Printf.printf "Starting iteration tests for ";
          Re.debug regex;
        );
        let start_time = Sys.time() (* Start measuring the time. *)
        in (* Iterate over words, count errors and time *)
        List.iter (fun (s, expected) -> if Re.matches regex s <> expected then errors := !errors + 1) words;
        total_time := !total_time +. (Sys.time() -. start_time);
        if debug then Printf.printf "Tests ended.\n\n";
      in


      (* TESTS *)
      iter_tests regex1 mals1 true;
      iter_tests regex2 mals2 true;
        

      (* 2. Porównanie r*r i rr* za pomocą [test_two]:
         - a*a i aa* na języku złożonym z liter a b 
         - (a|b)*(a|b) i (a|b)(a|b)* na języku złożonym z a b c *)
      
      let lang1 = make_lang "ab" 1000
      and lang2 = make_lang "abc" 1000
      and regex3 = Re.re "a*a"
      and regex4 = Re.re "aa*"
      and regex5 = Re.re "(a|b)*(a|b)" 
      and regex6 = Re.re "(a|b)(a|b)*" 
      in

      (* TESTS 3-4 *)
      let err, time = test_two regex3 regex4 lang1 true
      in
      errors := !errors + err;
      total_time := !total_time +. time;

      (* TESTS 5-6 *)
      let err, time = test_two regex5 regex6 lang2 true
      in
      errors := !errors + err;
      total_time := !total_time +. time;
  
      (* 3. Porównanie r1|r2 i r2|r1 za pomocą [test_two]:
         - a(a|b)*|(a|b)*b oraz (a|b)*b|a(a|b)* na napisach złożonych z a b *)
      
      let regex7 = Re.re "a(a|b)*|(a|b)*b" 
      and regex8 = Re.re "(a|b)*b|a(a|b)*" 
      in
      (* TESTS 7-8 *)
      let err, time = test_two regex7 regex8 lang1 true
      in
      errors := !errors + err;
      total_time := !total_time +. time;
  
      (* 4. Jedna lub dwie własne nietrywialne propozycje. *)
      
      (* Test for difference between '|' and '*' *)
      let regex9 = Re.re "(a|b|c)*(d|e)*(a|b|c)*" 
      and regex10 = Re.re "(a*b*c*)*(d*e*)*(a*b*c*)*"
      and lang3 = make_lang "abcde" 1000
      in

      (* TESTS 9-10 *)
      let err, time = test_two regex9 regex10 lang3 true
      in
      errors := !errors + err;
      total_time := !total_time +. time;

      (* Same but tougher. *)
      let regex11 = Re.re "((a|b|c)*(c|d|e))*" 
      and regex12 = Re.re "((a*b*c*)*c|(a*b*c*)*d|(a*b*c*)*e)*"
      in

      (* TESTS 11-12 *)
      let err, time = test_two regex11 regex12 lang3 true
      in
      errors := !errors + err;
      total_time := !total_time +. time;
      
      (* Bad Test (not included in final error count) *)
      Printf.printf "\nTest with an intentional error\n\n";
      let lang4 = make_lang "ab" 1000
      and regex13 = Re.re "a*" 
      and regex14 = Re.re "b*" 
      in
      (* TESTS 13-14 *)
      let _, time = test_two regex13 regex14 lang4 true
      in
      total_time := !total_time +. time;
  
      (* Wynik końcowy *)
      (!errors, !total_time)
end
