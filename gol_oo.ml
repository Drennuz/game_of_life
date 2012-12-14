class cell (status: bool) = 
    object
        val alive = status
        method isAlive () = alive 
    end;;

class virtual absWorld ilen jlen= 
    object (self)
        val cellarray = Array.make_matrix ilen jlen (new cell false) 
        val cellsize = 20
        method virtual nextGen: unit -> unit 
        method getCellSize = cellsize
        method setCell (i,j) c = cellarray.(i).(j) <- c
        method getCell (i,j) = cellarray.(i).(j)
        method display () = 
            for i = 0 to (ilen-1) do
                for j = 0 to (jlen-1) do
                    let c = self#getCell (i,j) in
                    if c#isAlive () then Graphics.set_color Graphics.black else Graphics.set_color Graphics.white;
                    Graphics.fill_rect (j * cellsize) ((ilen - i - 1) * cellsize) cellsize cellsize
                done
            done
    end;;

class world il jl = 
    object (self)
        inherit absWorld il jl
        method private countNeighbor (i, j) = 
            let count = ref 0 in
            for i1 = (i-1) to (i+1) do
                let k1 = (i1+il) mod il in
                for j1 = (j-1) to (j+1) do
                    let k2 = (j1+jl) mod jl in
                    if (self#getCell (k1,k2))#isAlive () then incr count done done;
            if (self#getCell (i,j))#isAlive () then decr count;
            !count
        method nextGen () = 
            for i = 1 to (il - 2) do
                for j = 1 to (jl - 2) do
                    let cneighbor = self#countNeighbor (i,j) in
                    if ((self#getCell (i,j))#isAlive ()) then (
                        if cneighbor > 3 then self#setCell (i,j) (new cell false);
                        if cneighbor < 2 then self#setCell (i,j) (new cell false))
                    else  
                        if cneighbor = 3 then self#setCell (i,j) (new cell true)
                done
            done
    end;;

exception End;;

let print_instruction () = print_string "enter initial % population\n";;

let main ilen jlen  = 
    if Array.length (Sys.argv) != 2 then print_instruction ()
    else
    let wd = new world ilen jlen and initCount = float_of_string (Sys.argv.(1)) in
        Random.init (int_of_float (Unix.time ()));
        for i = 0 to (int_of_float (initCount *. (float_of_int ilen) *. (float_of_int jlen))) do
            let rint = Random.int (ilen * jlen) in 
            wd#setCell (rint / jlen, rint mod jlen) (new cell true) done;
        Graphics.open_graph (" " ^ (string_of_int (jlen * wd#getCellSize)) ^ "x" ^ (string_of_int (ilen * wd#getCellSize)));
        wd#display ();
        while true do
            let e = Graphics.wait_next_event [Graphics.Key_pressed] in match e.Graphics.key with
            'n' -> wd#nextGen (); wd#display () 
            |'q' -> Graphics.close_graph () 
            |_ -> () done;;

let wdx = 40;;
let wdy = 40;;

main wdx wdy ;;
