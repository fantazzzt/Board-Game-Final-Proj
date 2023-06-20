type object_phrase = string list
 
type deploy_command =
 | TryAgain
 | ViewBoard
 | View of object_phrase
 | Deploy of object_phrase
 | Quit
 
(* type before_attack = 
 | TryAgain
 | ViewBoard
 | View of object_phrase
 | Yes
 | No

 type after_attack =
 | Territory of object_phrase
 | NumDice of string *)

exception Empty
 
exception Malformed
 
let rec parse_object_phrase str =
 List.filter (fun s -> s <> "") (String.split_on_char ' ' str)
 
let parse_deploy str =
 match String.(str |> trim |> lowercase_ascii) with
 | "quit" -> Quit
 | "view board" -> ViewBoard
 | "" -> raise Empty
 | s when String.sub s 0 7 = "deploy " ->
     let obj_phrase =
       String.sub s 7 (String.length s - 7) |> String.trim
     in
     Deploy (parse_object_phrase obj_phrase)
 | v when String.sub v 0 5 = "view " ->
     let view_phrase =
      String.sub v 5 (String.length v - 5) |> String.trim
     in
     View (parse_object_phrase view_phrase)
 | _ -> raise Malformed

let parse_names str = let nameslst = parse_object_phrase (String.trim str) in
 match List.length nameslst with
   | 3 -> nameslst
   | 0 -> raise Empty
   | _ -> raise Malformed


(* Called bedfore an attack says 'Yes' to attack - pattern match with type
(* before_attack *)
let parse_afterattack = raise (Failure "Unimplemented") *)

(* Called only after a player decides to attack - pattern match with type
after_attack *)