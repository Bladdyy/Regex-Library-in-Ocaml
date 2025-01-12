open TestRe
open Regexp

module Simple: REGEXP = struct
  include ReSimple
end

module Kit : REGEXP = struct
  include ReKit
end


let () =
  let tests (module Mod : REGEXP) name = 
    let module Tester = TestRegexp.Test(Mod)
    in
    Printf.printf "Running tests for %s...\n\n" name;
    let errors, time = Tester.test () in
    Printf.printf "Test completed with %d errors in %.2f seconds\n" errors time;
  in
  tests (module Simple : REGEXP) "Simple";
  Printf.printf "---------------------------------------------------\n";
  tests (module Kit : REGEXP) "Kit"
