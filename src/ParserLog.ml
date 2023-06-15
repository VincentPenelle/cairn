(* Type representing parse trees*)
type tree =
  | NonTerminal of string * int * tree list
    (*an inner node with the non-terminal name, the state number reached when read, and the list of its children*)
  | Terminal of string * int * string
(*a leaf with the terminal name, the state number reached when read, and the string composing this terminal in the parsed text.*)

let get_token = function
  | NonTerminal (token, _, _) | Terminal (token, _, _) -> token

let get_state = function
  | NonTerminal (_, state, _) | Terminal (_, state, _) -> state

let get_children = function NonTerminal (_, _, l) -> l | _ -> []

(* Gets the part of the input that corresponds to this tree.*)
let rec get_text = function
  | Terminal (_, _, text) -> text
  | NonTerminal (_, _, l) ->
      List.fold_left (fun acc ch -> acc ^ " " ^ get_text ch) "" l

type action =
  | ShiftRead of int * string * string
  | Input of string
  | Reduce of string * string * int * int
  | Accept
  | Reject
  | Error of string * string * string

let string_of_action = function
  | ShiftRead (n, t, s) ->
      "Shifting lookahead ( token " ^ t ^ " ) to state " ^ string_of_int n
      ^ " and reading " ^ s
  | Input s -> "Reading symbol " ^ s
  | Reduce (_, prod, state, _) ->
      "Reducing < " ^ prod ^ " > to state " ^ string_of_int state
  | Accept -> "Accepting"
  | Reject -> "Rejecting"
  | Error (loc, ind, mess) ->
      let loc =
        List.fold_left
          (fun acc s -> acc ^ s)
          ""
          (List.tl (String.split_on_char ',' loc))
      in
      "Error: " ^ mess ^ " (" ^ loc ^ " " ^ ind ^ ")."

type configuration = Stack of action * string * tree list

let isReading = function Stack (Input _, _, _) -> true | _ -> false
let isError = function Stack (Error _, _, _) -> true | _ -> false
let get_action = function Stack (a, _, _) -> a
let configuration_length = function Stack (_, _, l) -> List.length l
let initial_configuration = Stack (Input "", "", [])

let get_top_state (Stack (_, _, list)) =
  match list with [] -> 0 | d :: _ -> get_state d

let pop_configuration = function
  | Stack (act, s, _ :: t) -> Stack (act, s, t)
  | st -> st

(* Separates the [num] first elements of the list.*)
let split_list num l =
  let rec aux num l acc =
    if num = 0 then (acc, l)
    else
      match l with
      | a :: t -> aux (num - 1) t (a :: acc)
      | [] -> failwith "malformed derivation sequence"
  in
  aux num l []

let apply_action (Stack (_, str, l)) = function
  | ShiftRead (n, t, s) ->
      Stack (ShiftRead (n, t, s), s, Terminal (t, n, str) :: l)
  | Input s -> Stack (Input s, s, l)
  | Reduce (nonTerm, prod, state, num) ->
      let lup, ldown = split_list num l in
      Stack
        ( Reduce (nonTerm, prod, state, num),
          str,
          NonTerminal (nonTerm, state, lup) :: ldown )
  | Accept -> Stack (Accept, str, l)
  | Reject -> Stack (Reject, str, l)
  | Error (loc, ind, mess) -> Stack (Error (loc, ind, mess), str, l)

(* Normalize the height of a tree representation by appending empty lines (with a branch) on top of it. Used to put all leaves at the same depth.*)
let rec normalize_tree_markup color utf8 height width list =
  if List.length list >= height then list
  else
    let str =
      String.make ((width / 2) - 1) ' '
      ^ (if utf8 then "│" else "|")
      ^ String.make (width - (width / 2)) ' '
    in
    normalize_tree_markup color utf8 height width
      ((if color then LTerm_text.[ B_fg LTerm_style.green; S str; E_fg ]
        else LTerm_text.[ S str ])
      :: list)

(* Creates the line for the origins of the branches leading to the children of a node to get a proper display of the start of branches.*)
let make_children_string utf8 sizes =
  if List.length sizes = 1 then
    let width = List.hd sizes in
    String.make ((width / 2) - 1) ' '
    ^ (if utf8 then "│" else "|")
    ^ String.make (width - (width / 2)) ' '
  else
    let size = List.fold_left ( + ) 0 sizes in
    let top_pos = (size / 2) - 1 in
    let rec compute_pos acc sizes sum =
      match sizes with
      | [] -> List.rev acc
      | a :: t -> compute_pos ((sum + (a / 2) - 1) :: acc) t (sum + a)
    in
    let positions = compute_pos [] sizes 0 in
    let first_pos = List.hd positions in
    let last_pos = List.hd (List.rev positions) in
    let others = List.tl (List.rev (List.tl positions)) in

    let rec aux str pos =
      if pos = size then str
      else
        let new_str =
          if pos < first_pos || pos > last_pos then " "
          else if pos = first_pos then if utf8 then "┌" else "/"
          else if pos = last_pos then if utf8 then "┐" else "\\"
          else if pos = top_pos then
            if List.mem pos others then if utf8 then "┼" else "+"
            else if utf8 then "┴"
            else "^"
          else if List.mem pos others then if utf8 then "┬" else "v"
          else if utf8 then "─"
          else "-"
        in
        aux (str ^ new_str) (pos + 1)
    in

    aux "" 0

(* Converts a tree into a markup list that LTerm can display.*)
let markup_of_tree color utf8 node =
  let rec aux color min_size node =
    let token = get_token node
    and state = get_state node
    and children = get_children node in
    if children = [] then
      let string = Format.sprintf "%s(%d) " token state in
      let text = get_text node in
      let length =
        max (max (String.length string) (String.length text)) min_size
      in
      let string =
        if String.length string < length then
          let l = length - String.length string in
          String.make (l / 2) ' ' ^ string ^ String.make (l - (l / 2)) ' '
        else string
      in
      let text =
        if String.length text < length then
          let l = length - String.length text in
          String.make (l / 2) ' ' ^ text ^ String.make (l - (l / 2)) ' '
        else text
      in

      ( length,
        [
          (if color then LTerm_text.[ B_fg LTerm_style.green; S string; E_fg ]
           else LTerm_text.[ S string ]);
          (if color then LTerm_text.[ B_fg LTerm_style.green; S text; E_fg ]
           else LTerm_text.[ S text ]);
        ] )
    else
      let string_node = Format.sprintf "%s(%d)" token state in
      let min_size =
        max min_size (String.length string_node) / List.length children
      in
      let res_children = List.map (aux false min_size) children in
      let size_children = List.map fst res_children in
      let size_node = List.fold_left ( + ) 0 size_children in
      let string_node =
        if String.length string_node > size_node then
          String.sub string_node 0 size_node
        else
          let l = size_node - String.length string_node in
          String.make (l / 2) ' ' ^ string_node ^ String.make (l - (l / 2)) ' '
      in
      let string_node =
        if color then LTerm_text.[ B_fg LTerm_style.green; S string_node; E_fg ]
        else LTerm_text.[ S string_node ]
      in
      let string_branch = make_children_string utf8 size_children in
      let string_branch =
        if color then
          LTerm_text.[ B_fg LTerm_style.green; S string_branch; E_fg ]
        else LTerm_text.[ S string_branch ]
      in
      let max_height =
        List.fold_left max 0 (List.map List.length (List.map snd res_children))
      in
      let str_children =
        List.map
          (fun (size, s) -> normalize_tree_markup color utf8 max_height size s)
          res_children
      in
      let tail =
        List.fold_left
          (fun acc l -> List.map2 ( @ ) acc l)
          (List.hd str_children) (List.tl str_children)
      in
      (size_node, string_node :: string_branch :: tail)
  in
  aux color 0 node

(*Appends two markups reprensenting trees, aligning them on their leaves so they are at the same level.*)
let rec fuse_markup (n1, l1) (n2, l2) =
  if List.length l1 = List.length l2 then (n1 + n2, List.map2 ( @ ) l1 l2)
  else if List.length l1 < List.length l2 then
    fuse_markup (n1, [ LTerm_text.S (String.make n1 ' ') ] :: l1) (n2, l2)
  else fuse_markup (n1, l1) (n2, [ LTerm_text.S (String.make n2 ' ') ] :: l2)

let markup_of_configuration color utf8 = function
  | Stack (act, s, list) ->
      ( string_of_action act,
        s,
        snd
          (List.fold_left fuse_markup (0, [])
             (List.rev
                (List.mapi
                   (fun i d ->
                     markup_of_tree (if i = 0 then color else false) utf8 d)
                   list))) )

let stringlist_of_configuration utf8 seq =
  let act, s, list = markup_of_configuration false utf8 seq in
  ( act,
    s,
    List.map
      (fun m -> Zed_string.to_utf8 (LTerm_text.to_string (LTerm_text.eval m)))
      list )

let stack_of_configuration = function
  | Stack (_, _, list) ->
      List.map
        (fun tree ->
          get_token tree ^ "(" ^ string_of_int (get_state tree) ^ ")")
        list

let print_configuration channel derivation =
  let act, s, str_d = stringlist_of_configuration true derivation in
  Format.fprintf channel "@[<v 0>%s@," act;
  if not (isReading derivation) then (
    Format.fprintf channel "@,";
    List.iter (Format.fprintf channel "%s@,") str_d;
    Format.fprintf channel "@,";
    Format.fprintf channel "Lookahed : %s@]@," s)

let print_configuration_list channel derivations =
  List.iter (print_configuration channel) derivations

(*start of ui*)

(* (*Old tentative for getting the stack difference based on the size -- was buggy on reduce whenever reducing rule of the form X -> X alpha because X was the same on both stacks, but took errors into account. Also was recomputing it instead of relying on reduce information.*)
   let get_stack_diff l1 l2 =
     let rec aux l1 l2 acc1 acc2 =
       if List.length l1 > List.length l2 then aux (List.tl l1) l2 (acc1 + 1) acc2
       else if List.length l1 < List.length l2 then
         aux l1 (List.tl l2) acc1 (acc2 + 1)
       else
         match (l1, l2) with
         | [], [] -> (acc1, acc2)
         | a :: t1, b :: t2 ->
             if a = b then (acc1, acc2) else aux t1 t2 (acc1 + 1) (acc2 + 1)
         | _ -> failwith "impossible"
     in
     aux l1 l2 0 0
*)

(*Gets the number of elements of two consecutive stacks that have been modified by the current configuration*)
let get_stack_diff (configuration : configuration) (previous : configuration) =
  match get_action configuration with
  | Reduce (_, _, _, n) -> (n, 1)
  | ShiftRead _ -> (0, 1)
  | Input _ ->
      (configuration_length previous - configuration_length configuration, 0)
  | _ -> (0, 0)

open Lwt

let shift_first_elem prev next =
  match !next with
  | _ :: _ :: _ ->
      prev := List.hd !next :: !prev;
      next := List.tl !next
  | _ -> ()

let utf8_char_of_string str =
  List.hd (Zed_string.to_raw_list (Zed_string.of_utf8 str))

type split_mode = Horizontal | Vertical

let derivations_explorer derivations state_displayer =
  let split_mode = ref Vertical in
  let coord_tree = ref LTerm_geom.{ col = 0; row = 0 } in
  let next_list = ref (List.tl derivations) in
  let prev_list = ref [ List.hd derivations ] in

  let rect_for_trees ui =
    let size = LTerm_ui.size ui in
    if !split_mode = Horizontal then
      let col_limit = 3 * size.cols / 4 and row_limit_1 = size.rows / 4 in
      let row_limit_2 = row_limit_1 + (3 * size.rows / 8) in
      LTerm_geom.
        ( { row1 = 3; col1 = 1; row2 = size.rows - 2; col2 = col_limit },
          {
            row1 = 3;
            col1 = col_limit + 1;
            row2 = row_limit_1;
            col2 = size.cols - 1;
          },
          {
            row1 = row_limit_1 + 1;
            col1 = col_limit + 1;
            row2 = row_limit_2;
            col2 = size.cols - 1;
          },
          {
            row1 = row_limit_2 + 1;
            col1 = col_limit + 1;
            row2 = size.rows - 2;
            col2 = size.cols - 1;
          } )
    else
      let row_limit = 3 * size.rows / 4 in
      let col_limit_1 = size.cols / 4 in
      let col_limit_2 = col_limit_1 + (3 * size.cols / 8) in
      LTerm_geom.
        ( { row1 = 3; col1 = 1; row2 = row_limit; col2 = size.cols - 1 },
          {
            row1 = row_limit + 1;
            col1 = 1;
            row2 = size.rows - 2;
            col2 = col_limit_1;
          },
          {
            row1 = row_limit + 1;
            col1 = col_limit_1 + 1;
            row2 = size.rows - 2;
            col2 = col_limit_2;
          },
          {
            row1 = row_limit + 1;
            col1 = col_limit_2 + 1;
            row2 = size.rows - 2;
            col2 = size.cols - 1;
          } )
  in

  let enclosing_rect rectangle =
    LTerm_geom.
      {
        row1 = row1 rectangle - 1;
        col1 = col1 rectangle - 1;
        row2 = row2 rectangle + 1;
        col2 = col2 rectangle + 1;
      }
  in

  let center_tree ui =
    let tree_rect, _, _, _ = rect_for_trees ui in
    let size = LTerm_geom.size_of_rect tree_rect in
    let last_deriv = List.hd (List.rev derivations) in
    let _, _, repr = stringlist_of_configuration false last_deriv in
    let num_rows = List.length repr
    and num_cols = try String.length (List.hd repr) with _ -> 0 in
    let max_row = LTerm_geom.rows size and max_col = LTerm_geom.cols size in
    coord_tree :=
      LTerm_geom.
        {
          row = min ((max_row / 2) + (num_rows / 2)) max_row;
          col = max ((max_col / 2) - (num_cols / 2)) 0;
        }
  in

  let rec loop ui =
    LTerm_ui.wait ui >>= function
    | LTerm_event.Key { code = Escape; _ } -> return ()
    | LTerm_event.Key { code = Char letter; _ } ->
        if letter = utf8_char_of_string "n" then
          shift_first_elem prev_list next_list;
        if letter = utf8_char_of_string "p" then
          shift_first_elem next_list prev_list;
        if letter = utf8_char_of_string "c" then center_tree ui;
        if letter = utf8_char_of_string "m" then
          if !split_mode = Horizontal then split_mode := Vertical
          else split_mode := Horizontal;
        if letter = utf8_char_of_string "b" then (
          next_list := List.tl derivations;
          prev_list := [ List.hd derivations ]);
        if letter = utf8_char_of_string "e" then (
          next_list := [ List.hd (List.rev derivations) ];
          prev_list := List.tl (List.rev derivations));
        if letter = utf8_char_of_string "a" then (
          shift_first_elem next_list prev_list;
          while
            List.length !prev_list > 1 && not (isError (List.hd !next_list))
          do
            shift_first_elem next_list prev_list
          done);
        if letter = utf8_char_of_string "z" then (
          shift_first_elem prev_list next_list;
          while
            List.length !next_list > 1 && not (isError (List.hd !next_list))
          do
            shift_first_elem prev_list next_list
          done);
        LTerm_ui.draw ui;
        loop ui
    | LTerm_event.Key { code = Down; control = c; _ } ->
        (coord_tree :=
           LTerm_geom.
             { !coord_tree with row = (!coord_tree.row - if c then 10 else 1) });
        LTerm_ui.draw ui;
        loop ui
    | LTerm_event.Key { code = Up; control = c; _ } ->
        (coord_tree :=
           LTerm_geom.
             { !coord_tree with row = (!coord_tree.row + if c then 10 else 1) });
        LTerm_ui.draw ui;
        loop ui
    | LTerm_event.Key { code = Left; control = c; _ } ->
        (coord_tree :=
           LTerm_geom.
             { !coord_tree with col = (!coord_tree.col + if c then 10 else 1) });
        LTerm_ui.draw ui;
        loop ui
    | LTerm_event.Key { code = Right; control = c; _ } ->
        (coord_tree :=
           LTerm_geom.
             { !coord_tree with col = (!coord_tree.col - if c then 10 else 1) });
        LTerm_ui.draw ui;
        loop ui
    | _ -> loop ui
  in

  let draw ui matrix =
    let size = LTerm_ui.size ui in
    let ctx = LTerm_draw.context matrix size in
    LTerm_draw.clear ctx;
    let action, lookahead, trees =
      markup_of_configuration
        ((not (isError (List.hd !next_list)))
        && not (isError (List.hd !prev_list)))
        true (List.hd !next_list)
    in
    LTerm_draw.draw_string_aligned ctx 0 H_align_center
      (Zed_string.of_utf8 action);
    LTerm_draw.draw_string_aligned ctx 1 H_align_center
      (Zed_string.of_utf8 ("Lookahead : " ^ lookahead));
    let tree_rect, state_rect, prev_stack_rect, next_stack_rect =
      rect_for_trees ui
    in
    LTerm_draw.draw_frame_labelled ctx (enclosing_rect tree_rect)
      ~alignment:H_align_center
      (Zed_string.of_utf8 "Partial derivation")
      LTerm_draw.Light;
    let ctx2 = LTerm_draw.sub ctx tree_rect in
    List.iteri
      (fun l s ->
        LTerm_draw.draw_styled ctx2
          (!coord_tree.row + l - List.length trees)
          !coord_tree.col (LTerm_text.eval s))
      trees;
    LTerm_draw.draw_frame_labelled ctx
      (enclosing_rect state_rect)
      ~alignment:H_align_center
      (Zed_string.of_utf8
         ("State "
         ^ string_of_int (get_top_state (List.hd !next_list))
         ^ " items"))
      LTerm_draw.Light;
    let ctx_state = LTerm_draw.sub ctx state_rect in
    let state = state_displayer (get_top_state (List.hd !next_list)) in
    List.iteri
      (fun l s ->
        LTerm_draw.draw_string_aligned ctx_state l H_align_center
          (Zed_string.of_utf8 s))
      state;
    LTerm_draw.draw_frame_labelled ctx
      (enclosing_rect prev_stack_rect)
      ~alignment:H_align_center
      (Zed_string.of_utf8 "Previous Stack")
      LTerm_draw.Light;
    let ctx_prev_stack = LTerm_draw.sub ctx prev_stack_rect in
    let stack_prev = stack_of_configuration (List.hd !prev_list) in
    let stack_next = stack_of_configuration (List.hd !next_list) in
    let red_style =
      LTerm_style.
        {
          foreground = Some lred;
          background = None;
          bold = None;
          underline = None;
          blink = None;
          reverse = None;
        }
    in
    let green_style =
      LTerm_style.
        {
          foreground = Some green;
          background = None;
          bold = None;
          underline = None;
          blink = None;
          reverse = None;
        }
    in
    let popped, pushed =
      get_stack_diff (List.hd !next_list) (List.hd !prev_list)
    in
    if stack_prev = [] then
      LTerm_draw.draw_string_aligned ctx_prev_stack
        (LTerm_geom.rows (LTerm_draw.size ctx_prev_stack) - 1)
        H_align_center (Zed_string.of_utf8 "⊥");
    List.iteri
      (fun l s ->
        LTerm_draw.draw_string_aligned ctx_prev_stack
          (LTerm_geom.rows (LTerm_draw.size ctx_prev_stack)
          - List.length stack_prev + l)
          H_align_center
          ?style:(if popped > l then Some red_style else None)
          (Zed_string.of_utf8 s))
      stack_prev;
    LTerm_draw.draw_frame_labelled ctx
      (enclosing_rect next_stack_rect)
      ~alignment:H_align_center
      (Zed_string.of_utf8 "Current Stack")
      LTerm_draw.Light;
    let ctx_next_stack = LTerm_draw.sub ctx next_stack_rect in
    if stack_next = [] then
      LTerm_draw.draw_string_aligned ctx_next_stack
        (LTerm_geom.rows (LTerm_draw.size ctx_next_stack) - 1)
        H_align_center (Zed_string.of_utf8 "⊥");
    List.iteri
      (fun l s ->
        LTerm_draw.draw_string_aligned ctx_next_stack
          (LTerm_geom.rows (LTerm_draw.size ctx_next_stack)
          - List.length stack_next + l)
          H_align_center
          ?style:(if pushed > l then Some green_style else None)
          (Zed_string.of_utf8 s))
      stack_next;
    LTerm_draw.draw_styled ctx (size.rows - 1) 0
      (LTerm_text.eval
         [
           B_bg LTerm_style.green;
           S "Esc";
           E_bg;
           S " exit ; ";
           B_bg LTerm_style.green;
           S "n p";
           E_bg;
           S " navigate derivations ; ";
           B_bg LTerm_style.green;
           S "b e";
           E_bg;
           S " got to begin/end ";
           B_bg LTerm_style.green;
           S "a z";
           E_bg;
           S " previous/next error ";
           B_bg LTerm_style.green;
           S "←↑→↓";
           E_bg;
           S " move tree (quick with ";
           B_bg LTerm_style.green;
           S "Ctrl";
           E_bg;
           S "); ";
           B_bg LTerm_style.green;
           S "c";
           E_bg;
           S " center view ; ";
           B_bg LTerm_style.green;
           S "m";
           E_bg;
           S " switch H/V split ";
         ]);
    ()
  in

  let main () =
    Lazy.force LTerm.stdout >>= fun term ->
    (* Coordinates of the message. *)
    LTerm_ui.create term (fun ui matrix -> draw ui matrix) >>= fun ui ->
    Lwt.finalize
      (fun () ->
        center_tree ui;
        loop ui)
      (fun () -> LTerm_ui.quit ui)
  in

  Lwt_main.run (main ())
