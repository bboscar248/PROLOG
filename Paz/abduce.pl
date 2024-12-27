% filepath: /C:/Users/luosc/Downloads/Datathon/PROLOG/Paz/programa_principal.pl
% Meta-intérprete para la abducción
abduce(true, E, E) :- !.
abduce((A, B), E0, E) :- !,
    abduce(A, E0, E1),
    abduce(B, E1, E).
abduce(A, E0, E) :-
    clause(A, B),
    abduce(B, E0, E).
abduce(A, E, E) :-
    member(A, E).
abduce(A, E, [A|E]) :-
    \+ member(A, E),
    abducible(A),
    \+ abduce_not(A, E, E).
abduce(not(A), E0, E) :-
    \+ member(A, E0),
    abduce_not(A, E0, E).

abduce_not((A, B), E0, E) :- !,
    abduce_not(A, E0, E);
    abduce_not(B, E0, E).
abduce_not(A, E0, E) :-
    setof(B, clause(A, B), L),
    abduce_not_list(L, E0, E).
abduce_not(A, E, E) :-
    member(not(A), E).
abduce_not(A, E, [not(A)|E]) :-
    \+ member(not(A), E),
    abducible(A),
    \+ abduce(A, E, E).
abduce_not(not(A), E0, E) :-
    \+ member(not(A), E0),
    abduce(A, E0, E).

abduce_not_list([], E, E).
abduce_not_list([B|Bs], E0, E) :-
    abduce_not(B, E0, E1),
    abduce_not_list(Bs, E1, E).

% Cargar un nuevo sistema
cargar_sistema :- 
    write('Ingrese el nombre del archivo del sistema a cargar (sin extensión): '),
    read(Archivo),
    atom_concat('C:/Users/luosc/OneDrive/Escritorio/Practica/Paz/', Archivo, Ruta),
    atom_concat(Ruta, '.pl', RutaCompleta),
    consult(RutaCompleta),
    write('Archivo '), write(Archivo), write('.pl cargado correctamente.'), nl.

% Cargar hechos observados desde un archivo
cargar_hechos :- 
    write('Ingrese el nombre del archivo de hechos observados (sin extensión): '),
    read(Archivo),
    atom_concat('C:/Users/luosc/OneDrive/Escritorio/Practica/Paz/', Archivo, Ruta),
    atom_concat(Ruta, '.pl', RutaCompleta),
    consult(RutaCompleta),
    write('Hechos observados cargados desde '), write(Archivo), write('.pl'), nl.

% Iniciar diagnóstico
iniciar_diagnostico :- 
    write('¿Qué observación has hecho? (por ejemplo, "no_arranca(X)")'), nl,
    read(Observacion),
    (   abduce(Observacion, [], Explicaciones)
    ->  escribir_explicaciones(Explicaciones)
    ;   write('No se encontraron explicaciones.'), nl
    ).

% Escribir las explicaciones obtenidas por abducción
escribir_explicaciones([]) :- 
    write('No se encontraron explicaciones.'), nl.

escribir_explicaciones([X | Xs]) :- 
    write('Posible causa: '), write(X), nl,
    escribir_explicaciones(Xs).

% Predicado principal para iniciar el programa manualmente
main :- 
    write('Bienvenido al sistema de diagnóstico de averías.'), nl,
    write('¿Desea cargar un nuevo sistema? (si/no) '),
    read(RespuestaSistema),
    (   RespuestaSistema == si -> cargar_sistema
    ;   true
    ),
    write('¿Desea cargar hechos observados desde un archivo? (si/no) '),
    read(RespuestaHechos),
    (   RespuestaHechos == si -> cargar_hechos
    ;   true
    ),
    (   RespuestaSistema == no, RespuestaHechos == no ->
        write('No se han proporcionado hechos observados ni se han preguntado efectos.'), nl
    ;   iniciar_diagnostico
    ),
    nueva_sesion.