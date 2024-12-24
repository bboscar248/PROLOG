% Diagnóstico interactivo para coches y otros dominios

% Diagnóstico inicial: solicita la observación del usuario
diagnostico :-
    write('Sistema de diagnóstico interactivo de fallas.\n'),
    write('¿Qué dominio quieres diagnosticar? (coche/cocina)\n'),
    read(Dominio),
    cargar_dominio(Dominio).

% Cargar el dominio específico
cargar_dominio(coche) :-
    consult('C:/Users/luosc/OneDrive/Escritorio/Practica/coche_model.pl'),
    write('Dominio coche cargado.\n'),
    iniciar_diagnostico_coche.

cargar_dominio(cocina) :-
    consult('C:/Users/luosc/OneDrive/Escritorio/Practica/kitchen_model.pl'),
    write('Dominio cocina cargado.\n'),
    iniciar_diagnostico_cocina.

cargar_dominio(_) :-
    write('Dominio no reconocido.\n').

% Diagnóstico para el coche
iniciar_diagnostico_coche :-
    write('Diagnóstico del coche iniciado.\n'),
    write('¿Qué observación has hecho? (Ejemplo: no_funciona_aire_acondicionado)\n'),
    read(Observacion),
    procesar_observacion_coche(Observacion).

% Diagnóstico para la cocina
iniciar_diagnostico_cocina :-
    write('Diagnóstico de la cocina iniciado.\n'),
    write('¿Qué observación has hecho? (Ejemplo: no_funciona_horno)\n'),
    read(Observacion),
    procesar_observacion_cocina(Observacion).

% Procesar la observación para el coche
procesar_observacion_coche(no_funciona_aire_acondicionado) :-
    write('Hemos identificado que la falla pertenece al subsistema: aire_acondicionado.\n'),
    pregunta_causas_aire_acondicionado.

procesar_observacion_coche(no_funciona_luz_delantera) :-
    write('Hemos identificado que la falla pertenece al subsistema: sistema_electrico.\n'),
    pregunta_causas_sistema_electrico.

procesar_observacion_coche(frenos_no_responden) :-
    write('Hemos identificado que la falla pertenece al subsistema: frenos.\n'),
    pregunta_causas_frenos.

% (Se agregan mas observaciones para el coche)
procesar_observacion_coche(no_arranca) :-
    write('Hemos identificado que la falla pertenece al subsistema: motor.\n'),
    pregunta_causas_motor.

procesar_observacion_coche(ruido_extrano_motor) :-
    write('Hemos identificado que la falla pertenece al subsistema: motor.\n'),
    pregunta_causas_motor.

procesar_observacion_coche(vehiculo_vibrante) :-
    write('Hemos identificado que la falla pertenece al subsistema: suspension.\n'),
    pregunta_causas_suspension.

procesar_observacion_coche(pierde_aceite) :-
    write('Hemos identificado que la falla pertenece al subsistema: motor.\n'),
    pregunta_causas_motor.

% Procesar la observación para la cocina
procesar_observacion_cocina(no_funciona_horno) :-
    write('Hemos identificado que la falla pertenece al subsistema: horno.\n'),
    pregunta_causas_horno.

procesar_observacion_cocina(refrigerador_no_enfria) :-
    write('Hemos identificado que la falla pertenece al subsistema: refrigerador.\n'),
    pregunta_causas_refrigerador.

% (Se agregan mas observaciones para la cocina)
procesar_observacion_cocina(placa_no_calienta) :-
    write('Hemos identificado que la falla pertenece al subsistema: placa_de_cocina.\n'),
    pregunta_causas_placa_de_cocina.

procesar_observacion_cocina(agua_no_caliente) :-
    write('Hemos identificado que la falla pertenece al subsistema: calentador_agua.\n'),
    pregunta_causas_calentador_agua.

% Preguntas para el subsistema aire acondicionado (coche)
pregunta_causas_aire_acondicionado :-
    write('Las posibles causas son:\n1. falta_gas_refrigerante\n2. compresor_averiado\n'),
    write('¿Cuál de estas causas crees que es la correcta? (Escribe el número o "no_se")\n'),
    read(Respuesta),
    procesar_respuesta_aire_acondicionado(Respuesta).

procesar_respuesta_aire_acondicionado(no_se) :-
    write('Lo entiendo, intentemos con más detalles.\n'),
    diagnostico_adicional_flujo_gas.

procesar_respuesta_aire_acondicionado(1) :-
    write('Probablemente el problema sea falta de gas refrigerante. Revisemos el sistema de gas.\n').

procesar_respuesta_aire_acondicionado(2) :-
    write('El compresor podría estar averiado. Continuemos con el diagnóstico.\n').

% Diagnóstico adicional para aire acondicionado (coche)
diagnostico_adicional_flujo_gas :-
    write('¿Sientes que el aire acondicionado tiene aire frío pero no suficiente? (si/no)\n'),
    read(Respuesta),
    (   Respuesta == si -> 
        write('Probablemente el problema es falta de gas refrigerante. Revisemos el sistema de gas.\n');
        write('Podría ser otro problema, sigamos diagnosticando el sistema de aire.\n')
    ).

% Preguntas para el subsistema eléctrico (coche)
pregunta_causas_sistema_electrico :-
    write('Las posibles causas son:\n1. fusible_quemado\n2. interruptor_averiado\n'),
    write('¿Cuál de estas causas crees que es la correcta? (Escribe el número o "no_se")\n'),
    read(Respuesta),
    procesar_respuesta_sistema_electrico(Respuesta).

procesar_respuesta_sistema_electrico(no_se) :-
    write('Lo entiendo, intentemos con más detalles.\n'),
    diagnostico_adicional_sistema_electrico.

procesar_respuesta_sistema_electrico(1) :-
    write('Probablemente el problema sea un fusible quemado. Reemplaza el fusible y prueba nuevamente.\n').

procesar_respuesta_sistema_electrico(2) :-
    write('El interruptor podría estar averiado. Revisa o reemplaza el interruptor.\n').

% Diagnóstico adicional para el sistema eléctrico (coche)
diagnostico_adicional_sistema_electrico :-
    write('¿Las luces del coche parpadean? (si/no)\n'),
    read(Respuesta),
    (   Respuesta == si -> 
        write('Podría haber un problema en el interruptor de encendido o el fusible. Reemplaza el fusible o revisa el interruptor.\n');
        write('Probablemente el problema está relacionado con otra causa del sistema eléctrico.\n')
    ).

% Preguntas para el subsistema frenos (coche)
pregunta_causas_frenos :-
    write('Las posibles causas son:\n1. pastillas_de_freno_danadas\n2. líquido_de_freno_bajo\n'),
    write('¿Cuál de estas causas crees que es la correcta? (Escribe el número o "no_se")\n'),
    read(Respuesta),
    procesar_respuesta_frenos(Respuesta).

procesar_respuesta_frenos(no_se) :-
    write('Lo entiendo, intentemos con más detalles.\n'),
    diagnostico_adicional_frenos.

procesar_respuesta_frenos(1) :-
    write('Probablemente el problema sea que las pastillas de freno están danadas. Reemplázalas.\n').

procesar_respuesta_frenos(2) :-
    write('Probablemente el problema sea que el líquido de freno está bajo. Revisa el nivel de líquido de freno.\n').

% Diagnóstico adicional para los frenos (coche)
diagnostico_adicional_frenos :-
    write('¿El pedal de freno se siente suave al pisarlo? (si/no)\n'),
    read(Respuesta),
    (   Respuesta == si -> 
        write('El problema podría estar relacionado con el líquido de freno. Revisa el nivel y repón si es necesario.\n');
        write('El problema podría ser las pastillas de freno. Verifica su estado.\n')
    ).

% Preguntas para el horno (cocina)
pregunta_causas_horno :-
    write('Las posibles causas son:\n1. fusible_quemado\n2. termostato_averiado\n'),
    write('¿Cuál de estas causas crees que es la correcta? (Escribe el número o "no_se")\n'),
    read(Respuesta),
    procesar_respuesta_horno(Respuesta).

procesar_respuesta_horno(no_se) :-
    write('Lo entiendo, intentemos con más detalles.\n'),
    diagnostico_adicional_horno.

procesar_respuesta_horno(1) :-
    write('Probablemente el problema sea un fusible quemado. Reemplaza el fusible y prueba nuevamente.\n').

procesar_respuesta_horno(2) :-
    write('El termostato podría estar averiado. Reemplázalo y prueba nuevamente.\n').

% Diagnóstico adicional para el horno (cocina)
diagnostico_adicional_horno :-
    write('¿El horno se calienta pero no alcanza la temperatura correcta? (si/no)\n'),
    read(Respuesta),
    (   Respuesta == si -> 
        write('Probablemente el problema sea el termostato. Reemplázalo y prueba nuevamente.\n');
        write('Probablemente el problema sea el fusible. Reemplázalo.\n')
    ).

% Preguntas para el refrigerador (cocina)
pregunta_causas_refrigerador :-
    write('Las posibles causas son:\n1. falta_gas_refrigerante\n2. motor_averiado\n'),
    write('¿Cuál de estas causas crees que es la correcta? (Escribe el número o "no_se")\n'),
    read(Respuesta),
    procesar_respuesta_refrigerador(Respuesta).

procesar_respuesta_refrigerador(no_se) :-
    write('Lo entiendo, intentemos con más detalles.\n'),
    diagnostico_adicional_refrigerador.

procesar_respuesta_refrigerador(1) :-
    write('Probablemente el problema sea falta de gas refrigerante. Revisa el sistema.\n').

procesar_respuesta_refrigerador(2) :-
    write('Probablemente el problema sea un motor averiado. Reemplázalo.\n').

% Diagnóstico adicional para el refrigerador (cocina)
diagnostico_adicional_refrigerador :-
    write('¿El refrigerador está encendido pero no enfría lo suficiente? (si/no)\n'),
    read(Respuesta),
    (   Respuesta == si -> 
        write('Probablemente el problema sea falta de gas refrigerante. Revisa el sistema.\n');
        write('El problema podría estar en el motor. Verifica su estado.\n')
    ).
