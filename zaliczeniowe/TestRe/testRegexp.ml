module Test(Re : REGEXP)  = struct

    (** {2 Elementy pomocnicze} *)
    
    (** Język to zbiór słów. Użyj modułu [Set] lub podobnego. *)
    type lang = Set.Make(String).t

    (** Tworzenie losowego zbioru słów z podanych liter. Do losowania należy 
        używać modułu [Random], parametr liczbowy powinien jakoś określać rozmiar 
        zbioru (im większa liczba tym większy zbiór). Może to być po prostu 
        liczba elementów zbioru, ale niekoniecznie. *)
    let make_lang str num =
      let rec gen_word todo acc =
        if todo <= 0 then acc
        else 
          let len = Random.int (2 * num + 8) and strlen = String.length str
          in
          let word = String.init len (fun _ -> str.[Random.int (strlen)])
          in
          set = StringSet.add word acc
          in
          if set.cardinal = acc.cardinal then gen_word todo acc
          else gen_word (todo - 1) (acc)
      in
      gen_word num, Set.Make(String)


      
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
      let start = Sys.time() 
      in
      let lang1 = select_accepted regex1 lang 
      and lang2 = select_accepted regex2 lang
      in
      let end = Sys.time()
      in
      if debug then (
        Printf.printf "TEMP\n";
      );
      (StringSet.equal lang1 lang2 then 0 else 1, end_time -. start_time)


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
      and mals = [
        ("baabaabaaab", false);
        ("baaaaaaab", true);
        ("aaaaaaa", false);
        ("aaaaab", true)
      ] 
      in
      List.iter (fun (s, expected) ->
        if Re.matches regex1 s <> expected then incr errors;
      ) mals;
  

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
      let err, time = test_two regex3 regex4 lang1 false 
      in
      errors := !errors + err;
      total_time := !total_time +. time;
      in
      let err, time = test_two regex5 regex6 lang2 false 
      in
      errors := !errors + err;
      total_time := !total_time +. time;
  
      (* 3. Porównanie r1|r2 i r2|r1 za pomocą [test_two]:
         - a(a|b)*|(a|b)*b oraz (a|b)*b|a(a|b)* na napisach złożonych z a b *)
      
      let regex7 = Re.re "a(a|b)*|(a|b)*b" 
      and regex8 = Re.re "(a|b)*b|a(a|b)*" 
      in
      let err, time = test_two regex7 regex8 lang1 false
      in
      errors := !errors + err;
      total_time := !total_time +. time;
  
      (* 4. Jedna lub dwie własne nietrywialne propozycje. *)
      let regex9 = Re.re "(a|b)*" 
      and regex10 = Re.re "(a|b|c)*"
      and lang3 = make_lang "abc" 500 
      in
      let err, time = test_two regex9 regex10 lang3 false
      in
      errors := !errors + err;
      total_time := !total_time +. time;
  
      (* Wynik końcowy *)
      (!errors, !total_time)
end
