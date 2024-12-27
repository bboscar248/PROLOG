saluda(Nom) :-
    format('Hola, ~w! Eres muy bonita!~n', [Nom]). 


% Format alternatiu: amb decoracions especials.
saluda_amb_decoracio(Nom) :-
    format('**********~n'),
    format('** Hola, ~w! **~n', [Nom]),
    format('** A tope amb el Prolog! **~n'),
    format('**********~n').