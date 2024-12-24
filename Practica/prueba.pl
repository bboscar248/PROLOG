% Diagnóstico interactivo
:- discontiguous procesar_observacion/1.

diagnostico :-
    writeln('Sistema de diagnostico interactivo de fallas del coche.'),
    writeln('¿Que observacion has hecho? (Ejemplo: no_funciona_aire_acondicionado)'),
    read(Observacion),
    procesar_observacion(Observacion).

% Procesa la observacion
procesar_observacion(no_funciona_aire_acondicionado) :-
    writeln('Hemos identificado que la falla pertenece al subsistema: aire_acondicionado'),
    writeln('¿Puede confirmar si la falla esta relacionada con alguna de estas causas?'),
    writeln('1. falta_gas_refrigerante'),
    writeln('2. compresor_averiado'),
    writeln('¿Cual de estas causas crees que es la correcta? (Escribe el numero o "no_se")'),
    read(Respuesta),
    manejar_respuesta(Respuesta).

% Manejar la respuesta del usuario
manejar_respuesta(1) :-
    writeln('Has seleccionado: falta_gas_refrigerante'),
    writeln('Realizando diagnostico adicional...').

manejar_respuesta(2) :-
    writeln('Has seleccionado: compresor_averiado'),
    writeln('Realizando diagnostico adicional...').

manejar_respuesta(no_se) :-
    writeln('Lo entiendo, intentemos con mas detalles.'),
    writeln('¿El aire acondicionado esta funcionando parcialmente? (si/no)'),
    read(Respuesta),
    procesar_parcialidad(Respuesta).

% Si el aire acondicionado esta funcionando parcialmente
procesar_parcialidad(si) :-
    writeln('Esto podria estar relacionado con falta_gas_refrigerante.'),
    writeln('Realizando diagnostico adicional...').

% Si el aire acondicionado no esta funcionando parcialmente
procesar_parcialidad(no) :-
    writeln('Esto podria estar relacionado con compresor_averiado.'),
    writeln('Realizando diagnostico adicional...').

% Segunda falla: luces delanteras no funcionan
procesar_observacion(no_funcionan_luces_delanteras) :-
    writeln('Hemos identificado que la falla pertenece al subsistema: sistema_electrico'),
    writeln('¿Puede confirmar si la falla esta relacionada con alguna de estas causas?'),
    writeln('1. fusible_quemado'),
    writeln('2. interruptor_defectuoso'),
    writeln('¿Cual de estas causas crees que es la correcta? (Escribe el numero o "no_se")'),
    read(Respuesta),
    manejar_respuesta_luces(Respuesta).

% Manejar la respuesta de luces delanteras
manejar_respuesta_luces(1) :-
    writeln('Has seleccionado: fusible_quemado'),
    writeln('Realizando diagnostico adicional...').

manejar_respuesta_luces(2) :-
    writeln('Has seleccionado: interruptor_defectuoso'),
    writeln('Realizando diagnostico adicional...').

manejar_respuesta_luces(no_se) :-
    writeln('Lo entiendo, intentemos con mas detalles.'),
    writeln('¿Las luces tienen parpadeos? (si/no)'),
    read(Respuesta),
    procesar_parpadeos(Respuesta).

% Si las luces parpadean
procesar_parpadeos(si) :-
    writeln('Esto podria estar relacionado con un problema de interruptor_defectuoso.'),
    writeln('Realizando diagnostico adicional...').

% Si las luces no parpadean
procesar_parpadeos(no) :-
    writeln('Esto podria estar relacionado con un fusible_quemado.'),
    writeln('Realizando diagnostico adicional...').

% Tercera falla: motor no arranca
procesar_observacion(no_arranca_motor) :-
    writeln('Hemos identificado que la falla pertenece al subsistema: motor'),
    writeln('¿Puede confirmar si la falla esta relacionada con alguna de estas causas?'),
    writeln('1. bateria_descarregada'),
    writeln('2. alternador_averiado'),
    writeln('¿Cual de estas causas crees que es la correcta? (Escribe el numero o "no_se")'),
    read(Respuesta),
    manejar_respuesta_motor(Respuesta).

% Manejar la respuesta del motor
manejar_respuesta_motor(1) :-
    writeln('Has seleccionado: bateria_descarregada'),
    writeln('Realizando diagnostico adicional...').

manejar_respuesta_motor(2) :-
    writeln('Has seleccionado: alternador_averiado'),
    writeln('Realizando diagnostico adicional...').

manejar_respuesta_motor(no_se) :-
    writeln('Lo entiendo, intentemos con mas detalles.'),
    writeln('¿Se escucha un sonido cuando intentas arrancar el motor? (si/no)'),
    read(Respuesta),
    procesar_sonido_motor(Respuesta).

% Si se escucha un sonido
procesar_sonido_motor(si) :-
    writeln('Esto podria estar relacionado con un problema de alternador_averiado.'),
    writeln('Realizando diagnostico adicional...').

% Si no se escucha un sonido
procesar_sonido_motor(no) :-
    writeln('Esto podria estar relacionado con una bateria_descarregada.'),
    writeln('Realizando diagnostico adicional...').

% Cuarta falla: ruedas pinchadas
procesar_observacion(ruedas_pinchadas) :-
    writeln('Hemos identificado que la falla pertenece al subsistema: ruedas'),
    writeln('¿Puede confirmar si la falla esta relacionada con alguna de estas causas?'),
    writeln('1. rueda_pincha'),
    writeln('2. valvula_danada'),
    writeln('¿Cual de estas causas crees que es la correcta? (Escribe el numero o "no_se")'),
    read(Respuesta),
    manejar_respuesta_ruedas(Respuesta).

% Manejar la respuesta de ruedas
manejar_respuesta_ruedas(1) :-
    writeln('Has seleccionado: rueda_pincha'),
    writeln('Realizando diagnostico adicional...').

manejar_respuesta_ruedas(2) :-
    writeln('Has seleccionado: valvula_danada'),
    writeln('Realizando diagnostico adicional...').

manejar_respuesta_ruedas(no_se) :-
    writeln('Lo entiendo, intentemos con mas detalles.'),
    writeln('¿La rueda tiene una perdida de aire visible? (si/no)'),
    read(Respuesta),
    procesar_pérdida_aire(Respuesta).

% Si la rueda tiene pérdida de aire
procesar_pérdida_aire(si) :-
    writeln('Esto podria estar relacionado con una rueda_pincha.'),
    writeln('Realizando diagnostico adicional...').

% Si la rueda no tiene pérdida de aire
procesar_pérdida_aire(no) :-
    writeln('Esto podria estar relacionado con una valvula_danada.'),
    writeln('Realizando diagnostico adicional...').

% Quinta falla: luces traseras no funcionan
procesar_observacion(no_funcionan_luces_traseras) :-
    writeln('Hemos identificado que la falla pertenece al subsistema: luces_traseras'),
    writeln('¿Puede confirmar si la falla esta relacionada con alguna de estas causas?'),
    writeln('1. bombilla_quemada'),
    writeln('2. cableado_defectuoso'),
    writeln('¿Cual de estas causas crees que es la correcta? (Escribe el numero o "no_se")'),
    read(Respuesta),
    manejar_respuesta_luces_traseras(Respuesta).

% Manejar la respuesta de luces traseras
manejar_respuesta_luces_traseras(1) :-
    writeln('Has seleccionado: bombilla_quemada'),
    writeln('Realizando diagnostico adicional...').

manejar_respuesta_luces_traseras(2) :-
    writeln('Has seleccionado: cableado_defectuoso'),
    writeln('Realizando diagnostico adicional...').

manejar_respuesta_luces_traseras(no_se) :-
    writeln('Lo entiendo, intentemos con mas detalles.'),
    writeln('¿Las luces traseras parpadean? (si/no)'),
    read(Respuesta),
    procesar_parpadeos_luces_traseras(Respuesta).

% Si las luces traseras parpadean
procesar_parpadeos_luces_traseras(si) :-
    writeln('Esto podria estar relacionado con un problema de cableado_defectuoso.'),
    writeln('Realizando diagnostico adicional...').

% Si las luces traseras no parpadean
procesar_parpadeos_luces_traseras(no) :-
    writeln('Esto podria estar relacionado con una bombilla_quemada.'),
    writeln('Realizando diagnostico adicional...').