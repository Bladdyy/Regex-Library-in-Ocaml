open TestRe
open Regexp

module Simple: REGEXP = struct
  include ReSimple
end

module Kit : REGEXP = struct
  include ReKit
end

module SimpleTester = TestRegexp.Test(Simple)
module KitTester = TestRegexp.Test(Kit)

let () =
  Printf.printf "Running tests for Simple...\n\n";
  let errors, time = SimpleTester.test () in
  Printf.printf "Test completed with %d errors in %.2f seconds\n" errors time;
  Printf.printf "---------------------------------------------------\n";
  Printf.printf "Running tests for Kit...\n\n";
  let errors, time = KitTester.test () in
  Printf.printf "Test completed with %d errors in %.2f seconds\n" errors time
