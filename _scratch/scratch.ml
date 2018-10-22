#require "cohttp-lwt-unix";;

#require "yojson";;



(* A bit overkill to open everything *)

open Cohttp

open Cohttp_lwt_unix

open Lwt.Infix

open Yojson

;;



(* Make a get call and return the body parsed as json.

 * You could use atdgen to produce the right marshalling

 * functions into nice records, but for such small data

 * structures you might as well do it by hands.

 *

 * val json_body : string -> Basic.json Lwt.t

 *)

let json_body uri =

  Client.get (Uri.of_string uri)  >>= fun (_resp, body) ->

  (* here you could check the headers or the response status

   * and deal with errors, see the example on cohttp repo README *)

  body |> Cohttp_lwt.Body.to_string >|= Yojson.Basic.from_string

;;



(* In utop the promise is realised immediately, so if you run the

 * function above, you'll get the result immediately *)

json_body "http://deckofcardsapi.com/api/deck/new/shuffle/?deck_count=1"

;;

(*Output:

- : Basic.json =

`Assoc

  [("shuffled", `Bool true); ("success", `Bool true); ("remaining", `Int 52);

   ("deck_id", `String "2svxenasd9qj")]

*)



(* In the code you will have more likely something like the following. *)

let shuffled_deck =

  json_body "http://deckofcardsapi.com/api/deck/new/shuffle/?deck_count=1"

;;

(* val shuffled_deck : Basic.json Lwt.t *)



(* To use it you just have to get use to the monadic bind, and once you

 * are ready, call `Lwt_main.run`. e.g. the kind of useless example below *)



(* val get_cards : Basic.json -> int -> Basic.json Lwt.t *)

let get_cards sdeck n =

  (* The use of Yojson is well explained in RWO *)

  let module YBU = Yojson.Basic.Util in

  let success = sdeck |> YBU.member "success" |> YBU.to_bool in

  if not success then Lwt.fail_with "Error while shuffling" else

    let deck_id = sdeck |> YBU.member "deck_id" |> YBU.to_string in

    Printf.sprintf "http://deckofcardsapi.com/api/deck/%s/draw/?count=%d" deck_id n

    |> json_body >>= fun cards ->

    Lwt.return cards



(* val do_something : unit -> string list Lwt.t *)

let do_something () =

  json_body "http://deckofcardsapi.com/api/deck/new/shuffle/?deck_count=1" >>= fun sdeck ->

  get_cards sdeck 3 >>= fun jcards ->

  let module YBU = Yojson.Basic.Util in

  let cards = jcards |> YBU.member "cards" |> YBU.to_list in

  List.map (fun c -> c |> YBU.member "code" |> YBU.to_string) cards

  |> Lwt.return



(* val do_something_else : unit -> unit Lwt.t *)

let do_something_else () =

  do_something () >>= fun cards ->

  List.iter (Printf.printf "%s\n") cards |> Lwt.return;;

;;



Lwt_main.run (do_something_else ()); print_endline "Done!"

;;



(* Be careful with the signatures, the example above can be re-run multiple

 * times, while the one below does nothing after the first run *)



(* val do_something_1 : string list Lwt.t *)

let do_something_1 =

  json_body "http://deckofcardsapi.com/api/deck/new/shuffle/?deck_count=1" >>= fun sdeck ->

  get_cards sdeck 3 >>= fun jcards ->

  let module YBU = Yojson.Basic.Util in

  let cards = jcards |> YBU.member "cards" |> YBU.to_list in

  List.map (fun c -> c |> YBU.member "code" |> YBU.to_string) cards

  |> Lwt.return



(* val do_something_else_1 : unit Lwt.t *)

let do_something_else_1 =

  do_something_1  >>= fun cards ->

  List.iter (Printf.printf "%s\n") cards |> Lwt.return;;

;;



Lwt_main.run do_something_else_1; print_endline "Done_1!"

;;

Lwt_main.run do_something_else_1; print_endline "Done_1!"

;;

Lwt_main.run do_something_else_1; print_endline "Done_1!"

;;

Lwt_main.run (do_something_else ()); print_endline "Done!"

;;

