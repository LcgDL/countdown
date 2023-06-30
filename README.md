# Countdown
A solution to the countdown problem

Given a target number, a set of source numbers which may be used at most once, and the 4 basic arithmetic operators (+, -, *, /), construct the whole number closest to the target. 

Example: Using the  numbers "[1, 3, 7, 10, 25, 50]" get all the possible combinations for the number:765, using the 4 operations .

main = print (solutions' [1, 3, 7, 10, 25, 50] 765)

λ> main
[3*((7*(50-10))-25)...]

λ> 3*((7*(50-10))-25)
765


Based on: 
https://wiki.haskell.org/Haskell_Quiz/Countdown
