# Verified Catenable Deque

> **[Purely Functional, Real-Time Deques with Catenation]** \
> by Haim Kaplan and Robert E. Tarjan \
> journal of the ACM 31:11-16 (1999) 1709-1723
> https://doi.org/10.1145/324133.324139

In this paper, a double-ended queue, a deque, is defined as a queue supporting
*push*, *inject*, *pop*, *eject*. *push* and *pop* repsectivelly add and remove
elements at the left of the queue. *inject* and *eject* are their counterpart at
the right of the queue.

Following the paper, we design a library containing several implementations of
deques supporting several operations in worst-case constant time ( /!\ strict not
amortized /!\ ):

| Module  | push | inject | pop  |      eject      |     append      |       rev       |         nth         |
|---------|:----:|:------:|:----:|:---------------:|:---------------:|:---------------:|:-------------------:|
| Deque   | O(1) |  O(1)  | O(1) |       O(1)      | :no_entry_sign: |       O(1)      | O(log(min(i, N-i))) |
| Steque  | O(1) |  O(1)  | O(1) | :no_entry_sign: |      O(1)       | :no_entry_sign: |   :no_entry_sign:   |
| Cadeque | O(1) |  O(1)  | O(1) |       O(1)      |      O(1)       | :no_entry_sign: |   :no_entry_sign:   |

All this implementations are verified correct using Rocq proof assistant.
