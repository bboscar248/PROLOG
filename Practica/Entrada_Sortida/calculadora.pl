calculadora :- 
    write('Benvingut a la calculadora interactiva!'), nl, 
    write('Introdueix el primer número: '), 
    read(Primer), 
    write('Introdueix el segon número: '),
    read(Segon), 
    write('Introdueix l\'operació (+, -, *, /): '), 
    read(Operacio), 
    processa_operacio(Primer, Segon, Operacio). 

processa_operacio(Primer, Segon, '+') :-
    Resultat is Primer + Segon, 
    format('Has introduit: ~w + ~w~n', [Primer, Segon]),
    format('El resultat és: ~w~n', [Resultat]).

processa_operacio(Primer, Segon, '-') :-
    Resultat is Primer - Segon, 
    format('Has introduit: ~w - ~w~n', [Primer, Segon]),
    format('El resultat és: ~w~n', [Resultat]).

processa_operacio(Primer, Segon, '*') :-
    Resultat is Primer * Segon, 
    format('Has introduit: ~w * ~w~n', [Primer, Segon]),
    format('El resultat és: ~w~n', [Resultat]).

processa_operacio(Primer, Segon, '/') :-
    (   Segon =\= 0
    ->  Resultat is Primer / Segon, 
        format('Has introduit: ~w * ~w~n', [Primer, Segon]),
        format('El resultat és: ~w~n', [Resultat])
    ;   write('No es pot dividir per zero!'), nl
    ).


processa_operacio(Primer, Segon, Operacio) :-
    format('Has introduiït: ~w ~w ~w~n', [Primer, Operacio, Segon]), 
    write('Operació no vàlida! Si us plau, introdueix una de les següents: +, -, *, /.').