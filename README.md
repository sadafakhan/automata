## Description

A sampling of Haskell functions implemented during an Automata Theory course. 

### `hammingDistance`

`hammingDistance` consists of three primary functions: 

* `SumInterval`: takes a non-empty list of numerical values and a positive integer n as input and computes the sum of every n values in the list, starting with the first.
* `divList`: takes a list and divides it into sublists of length n. The collection of these sublists are returned within a list.
* `hammingDist`: takes two strings of the same length as input and computes the Hamming distance of the two strings. It returns -1 if the strings are of different length.


### `PCF`

`PCF` implements an interpreter for a 'Programming language for Computable Functions' (PCF) environment. It takes files that hold a PCF expression and outputs the result, like a calculator. It consists of many functions, all parsing/interpreter based, to execute this. 