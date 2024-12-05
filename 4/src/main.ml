let file_name = "input"

let word = "MAS"

let parse file = 
        let line0 = input_line file in
        let width = String.length line0 in

        let height = 
                let rec height res = 
                        try 
                                ignore (input_line file);
                                height (res+1)
                        with
                        | End_of_file -> res
                in height 1
        in

        let res = Array.make_matrix height width '.' in

        seek_in file 0;

        for i=0 to height - 1 do
                let line = input_line file in
                Printf.eprintf "%s\n" line;
                for j=0 to width - 1 do
                        res.(i).(j) <- line.[j]
                done
        done;
        
        close_in file;

        res,height,width


let _ =
        let data,height,width = open_in file_name |> parse in
        
        let len = String.length word - 1 in
        
        let rec count l res = match l with
          | (i,j)::t ->
                let rec found i j di dj k =
                        k > len || 
                                begin
                                        (try word.[k] == data.(i+k*di).(j+k*dj) with _ -> false) &&
                                        found i j di dj (k+1)
                                end
                in
                (res + 
                 if 
                   (found i j 1 1 0 && found i (j+len) 1 (-1) 0) ||
                   (found i j 1 1 0 && found (i+len) j (-1) 1 0) ||
                   (found (i+len) (j+len) (-1) (-1) 0 && found (i+len) j (-1) 1 0) ||
                   (found (i+len) (j+len) (-1) (-1) 0 && found i (j+len) 1 (-1) 0)
                 then (Printf.eprintf "Found %i %i\n" i j ; 1) else 0) |> count t
          | [] -> res
        in

        let l = List.init height (function i -> List.init width (function j -> (i,j))) |> List.concat in

        (*let rec compute di dj res = 
          if di == 0 && dj == 0 then
            compute 1 0 res
          else if di > 1 then
            compute (-1) (dj+1) res
          else if dj > 1 then res
          else (Printf.eprintf "-------------------\n%i %i\n-------------------\n" di dj ;count l di dj res |> compute (di+1) dj)
        in*)

        count l 0 |> Printf.printf "Result = %i\n" 
        
