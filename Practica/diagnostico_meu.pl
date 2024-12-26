% Diagnóstico interactivo para coches y otros dominios

% Diagnóstico inicial: solicita la observación del usuario
diagnostico :-
    write('Sistema de diagnóstico interactivo de fallas.\n'),
    write('¿Qué dominio quieres diagnosticar? (coche/cocina)\n'),
    read(Dominio),
    cargar_dominio(Dominio).

% Cargar el dominio específico
cargar_dominio(coche) :-
    consult('C:/Users/luosc/OneDrive/Escritorio/Practica/coche1_model.pl'),  % Se carga el archivo coche1_model.pl
    write('Dominio coche cargado.\n'),
    iniciar_diagnostico(coche).

cargar_dominio(cocina) :-
    consult('C:/Users/luosc/OneDrive/Escritorio/Practica/kitchen1_model.pl'),  % Se carga el archivo cocina1_model.pl
    write('Dominio cocina cargado.\n'),
    iniciar_diagnostico(cocina).

cargar_dominio(_) :-
    write('Dominio no reconocido.\n').

% Diagnóstico para el coche
iniciar_diagnostico(coche) :-
    write('Diagnóstico del coche iniciado.\n'),
    write('¿Qué observación has hecho? (Ejemplo: no_funciona_aire_acondicionado)\n'),
    read(Observacion),
    procesar_observacion(coche, Observacion).

% Diagnóstico para la cocina
iniciar_diagnostico(cocina) :-
    write('Diagnóstico de la cocina iniciado.\n'),
    write('¿Qué observación has hecho? (Ejemplo: no_funciona_horno)\n'),
    read(Observacion),
    procesar_observacion(cocina, Observacion).

% Procesar la observación para el coche
procesar_observacion(coche, no_funciona_aire_acondicionado) :-
    write('Hemos identificado que la falla pertenece al subsistema: aire_acondicionado.\n'),
    pregunta_causas_aire_acondicionado.

procesar_observacion(coche, no_funciona_luz_delantera) :-
    write('Hemos identificado que la falla pertenece al subsistema: sistema_electrico.\n'),
    pregunta_causas_sistema_electrico.

procesar_observacion(coche, frenos_no_responden) :-
    write('Hemos identificado que la falla pertenece al subsistema: frenos.\n'),
    pregunta_causas_frenos.

% Procesar la observación para la cocina
procesar_observacion(cocina, no_funciona_horno) :-
    write('Hemos identificado que la falla pertenece al subsistema: horno.\n'),
    pregunta_causas_horno.

procesar_observacion(cocina, refrigerador_no_enfria) :-
    write('Hemos identificado que la falla pertenece al subsistema: refrigerador.\n'),
    pregunta_causas_refrigerador.

% (Aquí van más observaciones y preguntas relacionadas con cada dominio)
