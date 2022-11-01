(* New var generator *)

let new_var (prefix : string) =
  let counter_var = ref 0 in
  fun () ->
    let ccv = !counter_var in
    incr counter_var;
    prefix ^ (string_of_int ccv)

let new_type_var = new_var "T"
