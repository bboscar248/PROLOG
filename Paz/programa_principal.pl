:- dynamic preguntada/1.
:- dynamic respuesta/1.
:- dynamic causas_probables/1.
:- dynamic evidencia/1.

% Preguntar si un efecto se presenta
preguntar(Efecto) :- 
    not(preguntada(Efecto)),
    (   respuesta(Efecto) -> assertz(evidencia(Efecto)), assertz(preguntada(Efecto))
    ;   respuesta(no(Efecto)) -> assertz(preguntada(Efecto))
    ;   write('¿Se presenta este problema/efecto: '),
        write(Efecto),
        write('? (si/no) '),
        read(Respuesta),
        (   Respuesta == si -> assertz(respuesta(Efecto)), assertz(evidencia(Efecto))
        ;   Respuesta == no -> assertz(respuesta(no(Efecto)))
        ),
        assertz(preguntada(Efecto))
    ).

preguntar(_) :- 
    true. % No hacer nada si ya fue preguntada

% Verificar si un efecto ya fue preguntado
preguntada(Efecto) :- 
    respuesta(Efecto).
preguntada(Efecto) :- 
    respuesta(no(Efecto)).

% Iniciar diagnóstico
iniciar :- 
    findall(Efecto, respuesta(Efecto), EfectosObservados),
    findall(no(Efecto), respuesta(no(Efecto)), EfectosNoObservados),
    append(EfectosObservados, EfectosNoObservados, TodosEfectos),
    forall(member(Efecto, TodosEfectos), assertz(preguntada(Efecto))),
    forall(member(Efecto, EfectosObservados), assertz(evidencia(Efecto))),
    write('Diagnóstico completado. Puede solicitar las posibles causas con "causas." y la explicación con "explicacion."'), nl.

% Identificar causas probables y mostrarlas
causas :- 
    findall(Causa, (causa(_, Causa), relacion(Causa, EfectosEsperados), member(Efecto, EfectosEsperados), respuesta(Efecto)), CausasDuplicadas),
    sort(CausasDuplicadas, CausasProbables),
    (   CausasProbables \= [] ->
        retractall(causas_probables(_)),
        assertz(causas_probables(CausasProbables)),
        write('Las posibles causas son: '), nl,
        forall(member(Causa, CausasProbables), 
            (   write('- '), write(Causa), nl
            )),
        write('Puede solicitar la explicación con "explicacion."'), nl
    ;   write('No se puede concluir ninguna causa con los síntomas proporcionados.'), nl
    ).

% Explicación del diagnóstico
explicacion :- 
    causas_probables(CausasProbables),
    write('Explicación del diagnóstico:'), nl,
    forall(member(Causa, CausasProbables), 
        (   write('Causa probable: '), write(Causa), nl,
            relacion(Causa, EfectosEsperados),
            findall(Efecto, (member(Efecto, EfectosEsperados), evidencia(Efecto)), Evidencias),
            write('Porque presenta estos efectos: '), write(Evidencias), nl
        )).

% Iniciar una nueva sesión
nueva_sesion :- 
    write('¿Desea iniciar una nueva sesión? (si/no) '),
    read(Respuesta),
    (   Respuesta == si -> iniciar
    ;   write('Sesión terminada.'), nl
    ).

% Cargar un nuevo sistema
cargar_sistema :- 
    write('Ingrese el nombre del archivo del sistema a cargar (sin extensión): '),
    read(Archivo),
    atom_concat('C:/Users/Chenhui/OneDrive/Documentos/GitHub/dataosc/PROLOG/Paz', Archivo, Ruta),
    atom_concat(Ruta, '.pl', RutaCompleta),
    consult(RutaCompleta),
    write('Archivo '), write(Archivo), write('.pl cargado correctamente.'), nl.

% Cargar hechos observados desde un archivo
cargar_hechos :- 
    write('Ingrese el nombre del archivo de hechos observados (sin extensión): '),
    read(Archivo),
    atom_concat('C:/Users/Chenhui/OneDrive/Documentos/GitHub/dataosc/PROLOG/Paz', Archivo, Ruta),
    atom_concat(Ruta, '.pl', RutaCompleta),
    consult(RutaCompleta),
    write('Hechos observados cargados desde '), write(Archivo), write('.pl'), nl.

% Predicado principal para iniciar el programa manualmente
main :- 
    write('Bienvenido al sistema de diagnóstico de averías.'), nl,
    write('¿Desea cargar un nuevo sistema? (si/no) '),
    read(Respuesta),
    (   Respuesta == si -> cargar_sistema
    ;   true
    ),
    write('¿Desea cargar hechos observados desde un archivo? (si/no) '),
    read(RespuestaHechos),
    (   RespuestaHechos == si -> cargar_hechos
    ;   true
    ),
    iniciar,
    nueva_sesion.