(* type declaration *)
type cell = {life: int; neighbors: int; changed: bool};;
type board = {width:int; height: int; state: cell array array};;

(* define constants *)
let total_height = 750;;
let total_width = 905;;
let width_life = total_width;;
let height_life = 565;;
let height_stat_box = total_height - height_life;;
let life_size = 10;;
let edge = 2;;
let hCount = (height_life - edge) / (life_size + edge);;
let wCount = (width_life - edge) / (life_size + edge);;

let sleepTime = 0.2;;

let color_live = Graphics.black;;
let color_death = Graphics.white;;

(* functions *)

let rec restart f arg = 
    try f arg with Unix.Unix_error(Unix.EINTR,_,_) -> restart f arg;;

let minisleep (sec:float) = ignore (restart (Unix.select [] [] []) sec);;

let generate_seed () = 
    let t = Sys.time () in
    let n = int_of_float (t *. 10000.0) in
    Random.init (n mod 100000);;

let generate_random_list max count = 
    let res = ref [] in
    while List.length !res < count do
        let n = Random.int max in if List.mem n !res then () else res := n :: (!res)
    done;
    !res;;

let initiate_abs seeds = 
    let emptyCell = {life = 0; neighbors = 0; changed = false} in
    let st = Array.make_matrix hCount wCount emptyCell in
    let bd = {width = wCount; height = hCount; state = st} and random_number = generate_random_list (hCount * wCount - 1) seeds in
    for k = 0 to (seeds - 1) do
        let i = (List.nth random_number k) / wCount and j = (List.nth random_number k) mod wCount in
        bd.state.(i).(j) <- {bd.state.(i).(j) with life = 1; changed = true}
    done; 
    bd;;

let update_board_graph bd = 
    for i = 0 to bd.height - 1 do
        for j = 0 to bd.width - 1 do 
            if bd.state.(i).(j).changed = true then
                (if bd.state.(i).(j).life = 0 then 
                    Graphics.set_color color_death else Graphics.set_color color_live;
                Graphics.fill_rect (edge + (edge+life_size) * j) (edge + (edge+life_size) * i) life_size life_size)
        done;
    done

let initiate_graph inputPercent = 
    Graphics.open_graph (" " ^ (string_of_int total_width) ^ "x" ^ (string_of_int (total_height)));
    let seed = int_of_float (inputPercent *. (float_of_int hCount) *. (float_of_int wCount)) in
    let bod = initiate_abs seed in
    update_board_graph bod;
    bod;;

exception InvalidValue;;

let update_cell bd i j = 
    let i_low = if i < 1 then (bd.height - 1) else (i - 1) 
    and i_high = if (i + 1) > (bd.height - 1) then 0 else (i+1)
    and j_low = if j < 1 then (bd.width - 1) else (j-1)
    and j_high = if (j+1) > (bd.width - 1) then 0 else (j+1) in
    let count = ref 0 in
    if bd.state.(i_low).(j_low).life = 1 then count:=!count + 1;
    if bd.state.(i_low).(j).life = 1 then count:=!count + 1;
    if bd.state.(i_low).(j_high).life = 1 then count:=!count + 1;
    if bd.state.(i).(j_low).life = 1 then count:=!count + 1;
    if bd.state.(i).(j_high).life = 1 then count:=!count + 1;
    if bd.state.(i_high).(j_low).life = 1 then count:=!count + 1;
    if bd.state.(i_high).(j).life = 1 then count:=!count + 1;
    if bd.state.(i_high).(j_high).life = 1 then count:=!count + 1;
    
    let newCell = {bd.state.(i).(j) with neighbors = !count} in
    match bd.state.(i).(j).life with
        0 -> (if !count == 3 then {newCell with life = 1; changed = true} else {newCell with changed = false})
        |1 -> (if ((!count < 2) || (!count > 3)) then {newCell with life = 0; changed = true} else {newCell with changed = false})
        |_ -> raise InvalidValue;;
        
let update_board bd = 
    for i = 0 to (bd.height - 1) do
        for j = 0 to (bd.width - 1) do
            bd.state.(i).(j) <- update_cell bd i j
        done;
    done;;
    

let go () = 
    if (Array.length Sys.argv) != 3 then print_string "usage: gol count seed\n" else    
    (let counter = int_of_string Sys.argv.(1) and input = float_of_string Sys.argv.(2) in
    let bod = initiate_graph input in
    for i = 0 to counter do 
        minisleep sleepTime; update_board bod; update_board_graph bod 
    done);;

(* main program *)

generate_seed ();;
go ();;

(* to be removed finally *)
ignore (Graphics.read_key ());;
