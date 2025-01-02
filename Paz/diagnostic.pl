:- dynamic preguntada/1.
:- dynamic respuesta/1.
:- dynamic causas_probables/1.
:- dynamic evidencia/1.
:- dynamic relacion/2.
:- dynamic hecho_observado/1.

% Preguntar si un efecto se presenta
preguntar(Efecto) :- 

    % Comprobar que la pregunta no haya sido preguntada antes
    not(preguntada(Efecto)),

    % Si el efecto no ha sido respondido previamente, se pregunta al usuario
    format('~n¿Se presenta este problema/efecto: ~w? (si/no) ', [Efecto]),
    
    % Leemos la respuesta del usuario, que puede ser si. o no.
    read(Respuesta),

    % Si la respuesta es si., registramos que el efecto está presente y lo añadimos como evidencia
    (   Respuesta == si -> 
        assertz(respuesta(Efecto)), 
        assertz(evidencia(Efecto))
    ;   
    
    % Si la respuesta es no., registramos que el efecto no está presente
    Respuesta == no -> 
        assertz(respuesta(no(Efecto)))
    ),

    % Finalmente, marcamos que la pregunta ha sido realizada
    assertz(preguntada(Efecto)).

% Cuando la pregunta ya fue preguntada, no hacemos nada
preguntar(_) :- 
    true.


% Verificar si un efecto ya fue preguntado
% Comprobamos las respuestas afirmativas
preguntada(Efecto) :- 
    respuesta(Efecto).

% Comprobamos las respuestas negativas
preguntada(Efecto) :- 
    respuesta(no(Efecto)).


% Iniciar diagnostico
iniciar :- 

    % Borrar toda la información anterior 
    retractall(preguntada(_)),
    retractall(causas_probables(_)),
    retractall(evidencia(_)),

    % Recoger todos los subsistemas
    findall(Subsistema, subsistema(_, Subsistema), Subsistemas),

    % Iterar sobre cada subsistema
    forall(member(Subsistema, Subsistemas), 
        (   
            % Enseñar el título para el subsistema actual
            format('~n~`=t~60|~nPreguntas para el subsistema: ~w~n~`=t~60|~n', [Subsistema]),
            
            % Recoger todos los efectos (averías) del subsistema 
            findall(Efecto, averia(Subsistema, Efecto), Efectos),

            % Si hay efectos, llamar a <preguntar> para cada efecto 
            (   Efectos \= [] ->
                forall(member(Efecto, Efectos), preguntar(Efecto))
            ;   % Si no hay efectos, enseñar un mensaje indicándolo 
                format('No hay efectos para el subsistema: ~w~n', [Subsistema])
            )
        )
    ),

    % Enseñar mensaje de finalización del diagnóstico
    format('~n~`=t~60|~nDiagnostico completado.~nPuede solicitar las posibles causas con "causas." y la explicacion con "explicacion."~n~`=t~60|~n'),
    
    % Esperar que el usuario haga algo
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
    % Hacemos un clean de todos los predicados para asegurarnos que el sistema comience desde 0
    retractall(preguntada(_)),
    retractall(respuesta(_)),
    retractall(causas_probables(_)),
    retractall(evidencia(_)),
    retractall(hecho_observado(_)),

    % Le damos la bienvenida al usuario
    format('~n~`=t~60|~nBienvenido al sistema de diagnostico de averias.~n~`=t~60|~n'),

    % Cargamos la base de conocimiento al sistema
    format('Cargue un sistema (ponga el nombre del archivo sin extension .pl): '),
    read(ArchivoSistema),
    atom_concat(ArchivoSistema, '.pl', RutaCompletaSistema),
    atom_concat('C:/Users/luosc/OneDrive/Escritorio/Practica/Paz/', RutaCompletaSistema, RutaSistema),
    consult(RutaSistema),

    % Mensaje para decir al usuario que el dominio ha sigo cargado correctamente
    format('Archivo ~w.pl cargado correctamente.~n', [ArchivoSistema]),

    % Preguntamos si quiere cargar hechos observados, es decir, directamente los efectos/averías que haya observado
    format('¿Desea cargar hechos observados desde un archivo? (si/no) '),

    % Leemos si la respuesta es si o no
    read(RespuestaHechos),
    (   RespuestaHechos == si -> cargar_hechos
    ;   iniciar
    ),

    % Después de haber ejecutado o bien el predicado cargar_hechos o bien el predicado iniciar, ejecutamos el predicado esperar_comando
    esperar_comando.


% Cargar hechos observados desde un archivo
cargar_hechos :- 

    % Cargamos los hechos observados al sistema
    format('Ingrese el nombre del archivo de hechos observados (sin extension .pl): '),
    read(Archivo),
    atom_concat(Archivo, '.pl', RutaCompleta),
    atom_concat('C:/Users/luosc/OneDrive/Escritorio/Practica/Paz/', RutaCompleta, Ruta),
    consult(Ruta),

    % Mensaje para decir al usuario que los hechos observados han sigo cargados correctamente
    format('Hechos observados cargados desde ~w.pl~n', [Archivo]),

    % Predicados para guardar los hechos observados como respuestas
    registrar_hechos_observados,

    % Iniciar el diagnóstico sin la necesidad de hacer preguntar al usuario
    iniciar_diagnostico.


% Registrar hechos observados como respuestas
registrar_hechos_observados :-

    % Limpiar hechos observados anteriormente
    retractall(hecho_observado(_)), 

    % Recogemos los efectos afirmativos 
    findall(Efecto, respuesta(Efecto), EfectosObservados),

    % Añadimos cada efecto observado como un hecho 
    forall(member(Efecto, EfectosObservados), assertz(hecho_observado(Efecto))).


% Iniciar diagnostico sin hacer preguntas al usuario
iniciar_diagnostico :- 

    % Borramos cualquier información anterior sobre preguntas, causas probables y evidencias 
    retractall(preguntada(_)),
    retractall(causas_probables(_)),
    retractall(evidencia(_)),

    % Recogemos todos los efectos observados cargados (respuestas afirmativas!)
    findall(Efecto, hecho_observado(Efecto), Efectos),

    % Añadimos los efectos observados como evidencia y los marcamos como preguntadas 
    forall(member(Efecto, Efectos), 
        (   assertz(evidencia(Efecto)), % Añadimos el efecto como evidencia
            assertz(preguntada(Efecto)) % Marcamos el efecto como preguntado
        )
    ),

    % Enseñamos un mensaje final indicando que el diagnóstico se ha completado 
    format('Diagnostico completado.~nPuede solicitar las posibles causas con "causas." y la explicacion con "explicacion."~n'),

    % Esperamos el siguiente comando del usuario 
    esperar_comando.


% Salir del bucle de comandos
salir :- 
    format('~nSesion terminada.~n'), !, halt.