% Caso base: la lista está vacía (no se muestra nada)
mostraLlista([], _).

% Llamada inicial de la función con el índice comenzando en 1
mostraLlista(Lista) :- 
    mostraLlista(Lista, 1).

% Regla recursiva con índice N
mostraLlista([Cabeza|Cola], N) :-
    format('% Element: ~w~n', [Cabeza]),  % Muestra el elemento actual
    Seguent is N + 1,  % Incrementa el índice
    mostraLlista(Cola, Seguent).  % Llama recursivamente para el siguiente elemento