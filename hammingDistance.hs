module Submission where


--1: sumInterval takes a non-empty list of numerical values and a positive integer n as input and computes the sum of every n values in the list, starting with the first.
sumInterval [] _ = 0
sumInterval (h:t) 1 = h + sumInterval t 1
sumInterval (h:t) n = head (head(divList (h:t) n)) + sumInterval (drop (n-1) t) n


--1(cont). divList takes a list and divides it into sublists of length n. The collection of these sublists are returned within a list. 
divList [] _ = []
divList (h:t) n = (take n (h:t)) : (divList (drop n (h:t)) n)


--2: hammingDist takes two strings of the same length as input and computes the Hamming distance of the two strings. It returns -1 if the strings are of different length. 
hammingDist [] [] = 0
hammingDist (c:s) (a:w) = if length(c:s) /= length(a:w)
                             then -1
                          else
                                if c /= a
                                   then 1 + hammingDist s w
                                else hammingDist s w 
