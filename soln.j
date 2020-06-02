NB. Probabilities
NB. AA-AA | AA-Aa | AA-aa | Aa-Aa | Aa-aa | aa-aa
NB. ------+-------+-------+-------+-------+------
NB.   1.0 |   1.0 |   1.0 |  0.75 |   0.5 |   0.0

probs =: 1.0 1.0 1.0 0.75 0.5 0.0
amts =: 16458 19422 17187 19710 19992 16795
soln =: +/ 2 * amts * probs