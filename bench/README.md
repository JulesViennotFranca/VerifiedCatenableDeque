# Benchmarks

This section contains information about the benchmarking of the OCaml code.

## Main benchmark

Les benchmarks principaux du code OCaml consistent à mesurer les temps d'exécutions d'un comportement type sur une structure donnée.
Ici, ces benchmarks seront fait sur les List, les Deques, les Steques, les Cadeques et les Seks, une librairie proposant une implémentation des ..., une structure polyvalente et efficace.

Pour chaque librairie, une base de donnée est créée. Les éléments de la base de donnée sont des structures construites aléatoirement en utilisant un sous-ensemble des fonctions push, pop, inject, eject et concat. L'idée est d'obtenir différentes structures qui peuvent être construitent par les utilisateurs.

Ensuite, on va mesurer les temps d'exécution (TODO: et les tailles en mémoire) de nos cinq opérations principales sur les structures de la base de donnée. On stocke les résultats dans des fichiers `.csv` dans le dossier `tmp`. On peut ensuite plotter les résultats à l'aide d'un script python dans `graphics.ipynb`.

Pour lancer ces benchmarks, on utilise la commande `make benchmark`. Attention, cela peut prendre plusieurs dizaines de minute. Voici les résultats possibles de ces benchmarks:

TODO

## List-like benchmark

The list-like benchmark measures the execution times
of simple operations on different structures.
Only three operations are tested:

- creating a structure containing the integers 1, 2, ..., 10 000 000 using the `push` function;
- computing the sum of the elements of the structure using a `fold_left`;
- computing the sum of the elements of the structure using the `pop` function.

These execution times are measured for `List`, `Deques.Deque`, `Deques.Steque` and `Deques.Cadeque`.

This benchmark can be executed with the command `make list_like_benchmark`, which takes approximately one minute. Here is an example of output from this benchmark:

```
List:
    make 10m : 0.176 s
   sum_foldl : 0.022 s
     sum_pop : 0.023 s

Deque:
    make 10m : 0.399 s
   sum_foldl : 0.026 s
     sum_pop : 0.255 s

Steque:
    make 10m : 0.421 s
   sum_foldl : 0.025 s
     sum_pop : 0.319 s

Cadeque:
    make 10m : 0.443 s
   sum_foldl : 0.025 s
     sum_pop : 0.447 s
```
