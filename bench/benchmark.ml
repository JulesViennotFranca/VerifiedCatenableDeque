open Printf

let () = Printf.printf "List:\n%!"
module A = ListLike.Test (ListLike.Lst)
let () = Printf.printf "\n%!"

let () = Printf.printf "Deque:\n%!"
module B = ListLike.Test (Cadeque.Deque)
let () = Printf.printf "\n%!"

let () = Printf.printf "Steque:\n%!"
module C = ListLike.Test (Cadeque.Steque)
let () = Printf.printf "\n%!"

let () = Printf.printf "Cadeque:\n%!"
module D = ListLike.Test (Cadeque)
let () = Printf.printf "\n%!"

module M = Map.Make(String)

let colors =
  let colors = M.empty in
  let colors = M.add "List" "#DB3AE0" colors in
  let colors = M.add "List rev" "#EE594D" colors in
  let colors = M.add "Deque" "#F72500" colors in
  let colors = M.add "Steque" "#BCC20A" colors in
  let colors = M.add "Cadeque" "#5C7AFF" colors in
  let colors = M.add "Cadeque rev" "#6338B3" colors in
  colors

let make_plot title make_points steps size datafile scriptfile outputfile =
  (* Compute the points. *)
  let names, times = make_points steps size in

  (* Write the result to a data file. *)
  let rec write_line oc times = match times with
    | [] -> ()
    | [t] -> fprintf oc "%f \n" t
    | t :: l -> fprintf oc "%f \t" t; write_line oc l
  in
  let oc = open_out datafile in
  List.iteri
    (fun i times -> fprintf oc "%i \t" i; write_line oc times)
    times;
  close_out oc;

  (* Write the gnuplot script. *)
  let oc = open_out scriptfile in
  fprintf oc "set terminal pngcairo\n";
  fprintf oc "set output '%s'\n" outputfile;
  fprintf oc "set title '%s'\n" title;
  fprintf oc "set xlabel 'length (number of elements)'\n";
  fprintf oc "set ylabel 'time (ms)'\n";
  fprintf oc "set key left top\n";
  List.iteri
    (fun i name ->
      fprintf oc "%s '%s' using 1:%i title '%s' with lines lt rgb '%s', \\\n"
        (if i = 0 then "plot" else "    ")
        datafile
        (i + 2)
        name
        (M.find name colors);
    )
    names;
  close_out oc;

  (* Run the gnuplot script *)
  let command = sprintf "gnuplot %s" scriptfile in
  ignore (Sys.command command)

let () =
  let steps = 1000 in

  let push_title = sprintf "Pushing one element" in
  make_plot push_title Push.make_points steps 100
            "bench/tmp/data_push.txt"
            "bench/tmp/plot_push.gnu"
            "bench/result/push.png";

  let inject_title = sprintf "Injecting one element" in
  make_plot inject_title Inject.make_points steps 100
            "bench/tmp/data_inject.txt"
            "bench/tmp/plot_inject.gnu"
            "bench/result/inject.png";

  let pop_title = sprintf "Poping one element" in
  make_plot pop_title Pop.make_points steps 100
            "bench/tmp/data_pop.txt"
            "bench/tmp/plot_pop.gnu"
            "bench/result/pop.png";

  let eject_title = sprintf "Ejecting one element" in
  make_plot eject_title Eject.make_points steps 100
            "bench/tmp/data_eject.txt"
            "bench/tmp/plot_eject.gnu"
            "bench/result/eject.png";

  let concat_title = sprintf "Concatenating with itself" in
  make_plot concat_title Concat.make_points steps 300
            "bench/tmp/data_concat.txt"
            "bench/tmp/plot_concat.gnu"
            "bench/result/concat.png"