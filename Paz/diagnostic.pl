:- dynamic preguntada/1.
:- dynamic respuesta/1.
:- dynamic causas_probables/1.
:- dynamic evidencia/1.
:- dynamic relacion/2.
:- dynamic sistema_cargado/1.
:- dynamic hecho_observado/1.

% Preguntar si un efecto se presenta
preguntar(Efecto) :- 
    not(preguntada(Efecto)),
    (   respuesta(Efecto) -> assertz(evidencia(Efecto)), assertz(preguntada(Efecto))
    ;   respuesta(no(Efecto)) -> assertz(preguntada(Efecto))
    ;   format('~n¿Se presenta este problema/efecto: ~w? (si/no) ', [Efecto]),
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

% Iniciar diagnostico
iniciar :- 
    retractall(preguntada(_)),
    retractall(causas_probables(_)),
    retractall(evidencia(_)),
    findall(Subsistema, subsistema(_, Subsistema), Subsistemas),
    forall(member(Subsistema, Subsistemas), 
        (   format('~n~`=t~60|~nPreguntas para el subsistema: ~w~n~`=t~60|~n', [Subsistema]),
            findall(Efecto, averia(Subsistema, Efecto), Efectos),
            (   Efectos \= [] ->
                forall(member(Efecto, Efectos), preguntar(Efecto))
            ;   format('No hay efectos para el subsistema: ~w~n', [Subsistema])
            )
        )
    ),
    format('~n~`=t~60|~nDiagnostico completado.~nPuede solicitar las posibles causas con "causas." y la explicacion con "explicacion."~n~`=t~60|~n'),
    esperar_comando.

% Identificar causas probables y mostrarlas
causas :- 
    findall(Causa, (causa(_, Causa), relacion(Causa, EfectosEsperados), member(Efecto, EfectosEsperados), respuesta(Efecto)), CausasDuplicadas),
    sort(CausasDuplicadas, CausasProbables),
    (   CausasProbables \= [] ->
        retractall(causas_probables(_)),
        assertz(causas_probables(CausasProbables)),
        format('~n~`=t~60|~nLas posibles causas son:~n~`=t~60|~n'),
        forall(member(Causa, CausasProbables), 
            (   format('  - ~w~n', [Causa])
            )),
        format('~nPuede solicitar la explicacion con "explicacion."~n')
    ;   format('~nNo se puede concluir ninguna causa con los sintomas proporcionados.~n')
    ).

% Explicacion del diagnostico
explicacion :- 
    causas_probables(CausasProbables),
    format('~n~`=t~60|~nExplicacion del diagnostico:~n~`=t~60|~n'),
    forall(member(Causa, CausasProbables), 
        (   format('Causa probable: ~w~n', [Causa]),
            relacion(Causa, EfectosEsperados),
            findall(Efecto, (member(Efecto, EfectosEsperados), evidencia(Efecto)), Evidencias),
            (   Evidencias \= [] ->
                format('Porque presenta estos efectos: ~w~n', [Evidencias])
            ;   format('No se encontraron efectos asociados para la causa: ~w~n', [Causa])
            )
        )).

% Esperar comando del usuario
esperar_comando :-
    format('~nIngrese "causas." para ver las causas probables, "explicacion." para ver la explicacion del diagnostico, o "nueva_sesion." para iniciar una nueva sesion.~n'),
    read(Comando),
    (   Comando == causas -> causas, esperar_comando
    ;   Comando == explicacion -> explicacion, esperar_comando
    ;   Comando == nueva_sesion -> nueva_sesion
    ;   Comando == salir -> salir
    ;   format('Comando no reconocido. Por favor, intente de nuevo.~n'), esperar_comando
    ).

% Iniciar una nueva sesion
nueva_sesion :- 
    format('~n¿Desea iniciar una nueva sesion? (si/no) '),
    read(Respuesta),
    (   Respuesta == si -> main
    ;   salir
    ).

% Predicado principal para iniciar el programa manualmente
main :- 
    retractall(preguntada(_)),
    retractall(respuesta(_)),
    retractall(causas_probables(_)),
    retractall(evidencia(_)),
    retractall(sistema_cargado(_)),
    retractall(hecho_observado(_)),
    format('~n~`=t~60|~nBienvenido al sistema de diagnostico de averias.~n~`=t~60|~n'),
    format('Cargue un sistema (ponga el nombre del archivo sin extension): '),
    read(ArchivoSistema),
    atom_concat('C:/Users/luosc/OneDrive/Escritorio/Practica/Paz/', ArchivoSistema, RutaSistema),
    atom_concat(RutaSistema, '.pl', RutaCompletaSistema),
    consult(RutaCompletaSistema),
    assertz(sistema_cargado(ArchivoSistema)),
    format('Archivo ~w.pl cargado correctamente.~n', [ArchivoSistema]),
    format('?Desea cargar hechos observados desde un archivo? (si/no) '),
    read(RespuestaHechos),
    (   RespuestaHechos == si -> cargar_hechos
    ;   iniciar
    ),
    esperar_comando.


% Cargar hechos observados desde un archivo
cargar_hechos :- 
    format('Ingrese el nombre del archivo de hechos observados (sin extension): '),
    read(Archivo),
    atom_concat('C:/Users/luosc/OneDrive/Escritorio/Practica/Paz/', Archivo, Ruta),
    atom_concat(Ruta, '.pl', RutaCompleta),
    consult(RutaCompleta),
    format('Hechos observados cargados desde ~w.pl~n', [Archivo]),
    registrar_hechos_observados,
    iniciar_diagnostico.

% Registrar hechos observados como respuestas
registrar_hechos_observados :-
    retractall(hecho_observado(_)), % Limpiar hechos observados anteriores
    findall(Efecto, respuesta(Efecto), EfectosObservados),
    forall(member(Efecto, EfectosObservados), assertz(hecho_observado(Efecto))).

% Iniciar diagnostico sin hacer preguntas
iniciar_diagnostico :- 
    retractall(preguntada(_)),
    retractall(causas_probables(_)),
    retractall(evidencia(_)),
    findall(Efecto, hecho_observado(Efecto), Efectos),
    forall(member(Efecto, Efectos), 
        (   assertz(evidencia(Efecto)), assertz(preguntada(Efecto))
        )
    ),
    format('Diagnostico completado.~nPuede solicitar las posibles causas con "causas." y la explicacion con "explicacion."~n'),
    esperar_comando.

% Salir del bucle de comandos
salir :- 
    format('~nSesion terminada.~n'), !, halt.