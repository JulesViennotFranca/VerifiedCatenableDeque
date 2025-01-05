open Printf

let () = Printf.printf "List:\n%!"
module A = ListLike.Bench (ListLike.Lst)
let () = Printf.printf "\n%!"

let () = Printf.printf "Deque:\n%!"
module B = ListLike.Bench (Cadeque.Deque)
let () = Printf.printf "\n%!"

let () = Printf.printf "Steque:\n%!"
module C = ListLike.Bench (Cadeque.Steque)
let () = Printf.printf "\n%!"

let () = Printf.printf "Cadeque:\n%!"
module D = ListLike.Bench (Cadeque)
let () = Printf.printf "\n%!"

let () = Printf.printf "Arthur Wendling's cadeque:\n%!"
module F = ListLike.Bench (Cadeque.ArtWend)
let () = Printf.printf "\n%!"

module type BENCH = sig
  val title : string

  val make_plot : int -> int -> (string * string) list * float list list
end

let display
  (module B : BENCH) steps size datafile scriptfile outputfile =

  (* Compute the points *)
  let plot_infos, times = B.make_plot steps size in

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
  fprintf oc "set title '%s'\n" B.title;
  fprintf oc "set xlabel 'length (number of elements)'\n";
  fprintf oc "set ylabel 'time (ms)'\n";
  fprintf oc "set key left top\n";

  List.iteri
    (fun i (name, color) ->
      fprintf oc "%s '%s' using 1:%i title '%s' with lines lt rgb '%s', \\\n"
        (if i = 0 then "plot" else "    ")
        datafile
        (i + 2)
        name
        color;
    )
    plot_infos;
  close_out oc;

  (* Run the gnuplot script *)
  let command = sprintf "gnuplot %s" scriptfile in
  ignore (Sys.command command)

let () =
  let steps = 1000 in

  display (module Push : BENCH) steps 100
            "bench/tmp/data_push.txt"
            "bench/tmp/plot_push.gnu"
            "bench/result/push.png";

  display (module Inject : BENCH) steps 100
            "bench/tmp/data_inject.txt"
            "bench/tmp/plot_inject.gnu"
            "bench/result/inject.png";

  display (module Pop : BENCH) steps 100
            "bench/tmp/data_pop.txt"
            "bench/tmp/plot_pop.gnu"
            "bench/result/pop.png";

  display (module Eject : BENCH) steps 100
            "bench/tmp/data_eject.txt"
            "bench/tmp/plot_eject.gnu"
            "bench/result/eject.png";

  display (module Concat : BENCH) steps 300
            "bench/tmp/data_concat.txt"
            "bench/tmp/plot_concat.gnu"
            "bench/result/concat.png";

  display (module Rdbench : BENCH) steps 1000
            "bench/tmp/data_random.txt"
            "bench/tmp/plot_random.txt"
            "bench/result/random.png"
