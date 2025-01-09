

let () =
  let input = SimpleRegexp.parse "(((XD | XDD)HELO))" in
  let boo = SimpleRegexp.to_string input in
  Printf.printf "Parsed successfully: %s\n" boo
