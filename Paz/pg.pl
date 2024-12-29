:- dynamic preguntada/1.
:- dynamic respuesta/1.
:- dynamic causas_probables/1.
:- dynamic evidencia/1.
:- dynamic relacion/2.
:- dynamic sistema_cargado/1.

% Preguntar si un efecto se presenta
preguntar(Efecto) :- 
    not(preguntada(Efecto)),
    (   respuesta(Efecto) -> assertz(evidencia(Efecto)), assertz(preguntada(Efecto))
    ;   respuesta(no(Efecto)) -> assertz(preguntada(Efecto))
    ;   format('~n?Se presenta este problema/efecto: ~w? (si/no) ', [Efecto]),
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
            ;   true
            )
        )
    ),
    format('~n~`=t~60|~nDiagnostico completado.~nPara solicitar las posibles causas y explicaciones, en la siguiente pregunta responda "no." y seguidamente escriba "causas." en caso de querer saber las causas y "explicacion." si quiere saber las explicaciones.~n~`=t~60|~n').

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
            format('Porque presenta estos efectos: ~w~n', [Evidencias])
        )).

% Esperar comando del usuario
esperar_comando :-
    format('~nIngrese "causas." para ver las causas probables o "explicacion." para ver la explicacion del diagnostico.~n'),
    read(Comando),
    (   Comando == causas -> causas, esperar_comando
    ;   Comando == explicacion -> explicacion, esperar_comando
    ;   true
    ).

% Iniciar una nueva sesion
nueva_sesion :- 
    format('~n?Desea iniciar una nueva sesion? (si/no) '),
    read(Respuesta),
    (   Respuesta == si -> main
    ;   format('~nSesion terminada.~n')
    ).

% Predicado principal para iniciar el programa manualmente
main :- 
    format('~n~`=t~60|~nBienvenido al sistema de diagnostico de averias.~n~`=t~60|~n'),
    format('Cargue un sistema (ponga el nombre del archivo sin extension): '),
    read(ArchivoSistema),
    atom_concat('C:/Users/Chenhui/OneDrive/Escritorio/Paz/', ArchivoSistema, RutaSistema),
    atom_concat(RutaSistema, '.pl', RutaCompletaSistema),
    consult(RutaCompletaSistema),
    assertz(sistema_cargado(ArchivoSistema)),
    format('Archivo ~w.pl cargado correctamente.~n', [ArchivoSistema]),
    format('?Desea cargar hechos observados desde un archivo? (si/no) '),
    read(RespuestaHechos),
    (   RespuestaHechos == si -> cargar_hechos
    ;   true
    ),
    iniciar,
    nueva_sesion.

% Cargar hechos observados desde un archivo
cargar_hechos :- 
    format('Ingrese el nombre del archivo de hechos observados (sin extension): '),
    read(Archivo),
    atom_concat('C:/Users/Chenhui/OneDrive/Escritorio/Paz/', Archivo, Ruta),
    atom_concat(Ruta, '.pl', RutaCompleta),
    consult(RutaCompleta),
    (   verificar_compatibilidad(Archivo) ->
        format('Hechos observados cargados desde ~w.pl~n', [Archivo]),
        iniciar
    ;   format('Error: Los hechos observados no son compatibles con el sistema cargado.~n'),
        cargar_hechos
    ).

% Verificar compatibilidad de los hechos observados con el sistema cargado
verificar_compatibilidad(ArchivoHechos) :-
    sistema_cargado(Sistema),
    atom_concat(Sistema, '_hechos', ArchivoHechos).