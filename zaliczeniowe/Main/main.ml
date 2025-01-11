open TestRe
open Regexp
(* Moduł zimplementowany dla SimpleRegexp *)

module SimpleRegexp : REGEXP = struct
  include ReSimple
end

module KitRegexp : REGEXP = struct
  include ReKit
end

module SimpleTester = TestRegexp.Test(SimpleRegexp)
module KitTester = TestRegexp.Test(KitRegexp)

let () =
  (* Uruchamianie testów dla SimpleRegexp *)
  Printf.printf "Running tests for SimpleRegexp...\n";
  let errors, time = SimpleTester.test () in
  Printf.printf "Test completed with %d errors in %.2f seconds\n" errors time;
  Printf.printf "---------------------------------------------------\n\n";
  Printf.printf "Running tests for KitRegexp...\n";
  let errors, time = KitTester.test () in
  Printf.printf "Test completed with %d errors in %.2f seconds\n" errors time