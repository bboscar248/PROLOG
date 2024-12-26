formatNumeros(X) :-
    format('Número sense decimals: ~0f~n', [X]), 
    format('Número amb decimals (1 xifra): ~1f~n', [X]),    
    format('Número amb decimals (2 xifres): ~2f~n', [X]), 
    format('Número amb decimals (3 xifres): ~3f~n', [X]).  