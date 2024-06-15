Hey! this is MARS,

A simple MAth paRSer,

### Grammar.ml
It handles (for now) the next rules : 

$S \rightarrow R+R|R-R|R*R|R/R|R!|R|q$

$R\rightarrow (S)$

Where $S$ is the initial state.

And q is a float.

Even though these rules are not really handy nor natural for a human, it works!

Use UTOP to use it.

Published in May 2024 but coded earlier this year and inspired by a school project of Regex Parser

### main.ml (not finished)
Handles basic math operations, as long as the priority is explicit:
3*(1+2) : OK
3*1+2 : not OK

Use UTOP to use it. (its easy and fun)

