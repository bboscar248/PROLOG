taulaDeMultiplicar(N) :- 
    taulaDeMultiplicar(N, 1). 

taulaDeMultiplicar(N, 11) :- 
    !. 

taulaDeMultiplicar(N, I) :-
    Resultat is N * I, 
    format('~w x ~w = ~w~n', [N, I, Resultat]), 
    Seguent is I + 1,
    taulaDeMultiplicar(N, Seguent).