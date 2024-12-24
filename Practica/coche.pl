% Causas posibles de fallos del coche
causa(bateria_descarregada(X)).
causa(motor_averiat(X)).
causa(sistema_electric_fallat(X)).
causa(roda_punxada(X)).
causa(diposit_buit(X)).


% Efectos o observaciones
efecto(no_arranca(X)).
efecto(llums_apagades(X)).
efecto(soroll_motor(X)).
efecto(cotxe_no_es_mou(X)).


% Regles que relacionan causes i efectes.
no_arranca(X) :- bateria_descarregada(X).
no_arranca(X) :- motor_averiat(X), soroll_motor(X).
no_arranca(X) :- sistema_electric_fallat(X), llums_apagades(X).
cotxe_no_es_mou(X) :- roda_punxada(X).
cotxe_no_es_mou(X) :- diposit_buit(X), no_arranca(X).
llums_apagades(X) :- bateria_descarregada(X).
llums_apagades(X) :- sistema_electric_fallat(X).
motor_averiat(X) :- cotxe_no_es_mou(X), \+ roda_punxada(X).


% Abducción: genera explicaciones para una observación
abduce(O, E) :-
    abduce(O, [], E).

% Caso base: si O es verdadero, no necesitamos más explicaciones.
abduce(true, E, E) :- !.

% Caso recursivo: si O es una conjunción (A y B), buscamos explicaciones para A y B
abduce((A, B), E0, E) :- !,
    abduce(A, E0, E1),
    abduce(B, E1, E).

% Si encontramos una regla que puede explicar A, buscamos más explicaciones
abduce(A, E0, E) :-
    clause(A, B),
    abduce(B, E0, E).

% Si A ya está en E (ya lo hemos considerado como una explicación), lo dejamos tal cual
abduce(A, E, E) :-
    member(A, E).

% Si A no está en E, lo añadimos a E si es abducible (puede ser una posible explicación)
abduce(A, E, [A | E]) :-
    \+ member(A, E),
    abducible(A).

% Definimos las fallas que son abducibles (es decir, que pueden ser causas)
abducible(bateria_descarregada(_)).
abducible(motor_averiat(_)).
abducible(sistema_electric_fallat(_)).
abducible(roda_punxada(_)).
abducible(diposit_buit(_)).



% Preguntar por la observación
preguntar_observacion :-
    write('¿Qué observación has hecho? (por ejemplo, "no_arranca(X)")'), nl,
    read(Observacion),
    (   efecto(Observacion)
    ->  abduce(Observacion, Explicaciones),
        escribir_explicaciones(Explicaciones)
    ;   write('Observación no válida. Intenta de nuevo.'), nl,
        preguntar_observacion
    ).

% Escribir las explicaciones obtenidas por abducción
escribir_explicaciones([]) :- 
    write('No se encontraron explicaciones.'). nl.

escribir_explicaciones([X | Xs]) :-
    write('Posible causa: '), write(X), nl,
    escribir_explicaciones(Xs).

% Iniciar el sistema de diagnóstico.
iniciar_diagnostico :-
    write('Sistema de diagnóstico de fallas de coches.'), nl,
    preguntar_observacion.
