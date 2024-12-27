% Domini dels cotxes
% Tenim 5 subsistemes: 
% direcció, transmissió, encesa, sistema elèctric, sistema de confort interior

% Subsistemes en bon funcionament
subsistema(coche, direccion).
subsistema(coche, transmission).
subsistema(coche, encesa).
subsistema(coche, sistema_electrico).
subsistema(coche, sistema_confort_interior).


% Funcionamento normal del sistema
% Relación entre componentes y comportamientos esperados

% Dirección
% Avarias/efectos/observaciones del subsistema de dirección

averia(direccion, vibraciones_volante).
averia(direccion, direccionamiento_erratico).
avaria(direccion, ruido_al_girar).
avaria(direccion, direccion_dura).
avaria(direccion, direccion_deficiente). 
averia(direccion, direccion_flacida).
averia(direccion, el_coche_no_gira).
averia(direccion, desviación_hacia_un_lado).

% Causas posibles de fallos del coche del subsistema de dirección
causa(direccion, falta_de_liquido_de_direccion).
causa(direccion, fallos_en_la_bomba_de_direccion_asistida). 
causa(direccion, desalineacion_de_las_ruedas).
causa(direccion, fallo_en_las_rotulas). 
causa(direccion, fallo_en_)



causa(direccion, falta_de_fluido_hidraulico). 
causa(direccion, bomba_hidraulica_averiada).
causa(direccion, desgaste_mangueras_hidraulicas).
causa(direccion, deterioro_de_las_juntas_y_sellos).
causa(direccion, desgaste_de_neumaticos). 
causa(direccion, caja_de_direccion_danada). 
causa(direccion, barra_de_direccion_danada).  
causa(direccion, erosion_de_soportes). 


% Reglas que relacionan causas y efectos del subsistema de dirección
relacion(falta_de_liquido_de_direccion, [direccion_deficiente]). 
relacion(fallos_en_la_bomba_de_direccion_asistida, [direccion_deficiente]).
relacion(desalineacion_de_las_ruedas, [direccion_deficiente]).


relacion(falta_de_fluido_hidraulico, [volante_vibrante, coche_no_gira, coche_gira_solo]).
relacion(bomba_hidraulica_averiada, [coche_no_gira]).
relacion(desgaste_mangueras_hidraulicas, [coche_no_gira]).
relacion(deterioro_de_las_juntas_y_sellos, [coche_no_gira]).
relacion(desgaste_de_neumaticos, [volante_vibrante]).
relacion(caja_de_direccion_danada, [coche_gira_solo]).
relacion(barra_de_direccion_danada, [coche_gira_solo]). 
relacion(erosion_de_soportes, [volante_desalineado]).


% Transmissió
% Avarias/efectos/observaciones del subsistema de transmisión
averia(transmission, dificultad_para_cambiar_marchas).
averia(transmission, perdida_de_potencia).
averia(transmission, ruidos_extraños_en_la_caja).
averia(transmission, vibraciones_en_el_piso).
averia(transmission, sobrecalentamiento).

% Causas posibles de fallos del coche del subsistema de transmisión
causa(transmission, nivel_bajo_de_aceite_transmision).
causa(transmission, desgaste_sincronizadores).
causa(transmission, embrague_desgastado).
causa(transmission, filtro_obstruido).
causa(transmission, fallo_en_la_bomba_de_aceite).

% Reglas que relacionan causas y efectos del subsistema de transmisión
relacion(nivel_bajo_de_aceite_transmision, [dificultad_para_cambiar_marchas, sobrecalentamiento]).
relacion(deshgaste_sincronizadores, [dificultad_para_cambiar_marchas]).
relacion(embrague_desgastado, [perdida_de_potencia, dificultad_para_cambiar_marchas]).
relacion(filtro_obstruido, [sobrecalentamiento]).
relacion(fallo_en_la_bomba_de_aceite, [ruidos_extraños_en_la_caja]).

% Encesa
% Avarias/efectos/observaciones del subsistema de encendido
averia(encesa, el_motor_no_arranca).
averia(encesa, ralentí_irregular).
averia(encesa, consumo_excesivo_combustible).
averia(encesa, detonaciones).
averia(encesa, perdida_de_potencia).

% Causas posibles de fallos del coche del subsistema de encendido
causa(encesa, bujias_desgastadas).
causa(encesa, cables_de_bujias_danados).
causa(encesa, bobina_de_encendido_fallida).
causa(encesa, fallo_del_sensor_de_cigueñal).
causa(encesa, distribuidor_defectuoso).

% Reglas que relacionan causas y efectos del subsistema de encendido
relacion(bujias_desgastadas, [el_motor_no_arranca, ralentí_irregular]).
relacion(cables_de_bujias_danados, [ralentí_irregular, perdida_de_potencia]).
relacion(bobina_de_encendido_fallida, [el_motor_no_arranca, detonaciones]).
relacion(fallo_del_sensor_de_cigueñal, [el_motor_no_arranca]).
relacion(distribuidor_defectuoso, [detonaciones, ralentí_irregular]).

% Sistema elèctric
% Avarias/efectos/observaciones del subsistema eléctrico
averia(sistema_electrico, luces_no_funcionan).
averia(sistema_electrico, bateria_descargada).
averia(sistema_electrico, alternador_fallido).
averia(sistema_electrico, cortocircuitos).
averia(sistema_electrico, fusibles_fundidos).

% Causas posibles de fallos del coche del subsistema eléctrico
causa(sistema_electrico, cableado_danado).
causa(sistema_electrico, bateria_anticuada).
causa(sistema_electrico, regulador_de_voltaje_defectuoso).
causa(sistema_electrico, conexiones_sueltas).
causa(sistema_electrico, alternador_dañado).

% Reglas que relacionan causas y efectos del subsistema eléctrico
relacion(cableado_danado, [cortocircuitos, luces_no_funcionan]).
relacion(bateria_anticuada, [bateria_descargada]).
relacion(regulador_de_voltaje_defectuoso, [alternador_fallido]).
relacion(conexiones_sueltas, [fusibles_fundidos]).
relacion(alternador_dañado, [bateria_descargada, alternador_fallido]).

% Sistema de confort interior
% Avarias/efectos/observaciones del subsistema de confort interior
averia(sistema_confort_interior, aire_acondicionado_no_funciona).
averia(sistema_confort_interior, asientos_no_se_ajustan).
averia(sistema_confort_interior, sistema_audio_no_funciona).
averia(sistema_confort_interior, ventanas_no_se_abren).
averia(sistema_confort_interior, calefacción_inadecuada).

% Causas posibles de fallos del coche del subsistema de confort interior
causa(sistema_confort_interior, fusible_fundido).
causa(sistema_confort_interior, motor_electrico_averiado).
causa(sistema_confort_interior, control_remoto_fallido).
causa(sistema_confort_interior, conductos_obstruidos).
causa(sistema_confort_interior, sensores_fallidos).

% Reglas que relacionan causas y efectos del subsistema de confort interior
relacion(fusible_fundido, [sistema_audio_no_funciona, ventanas_no_se_abren]).
relacion(motor_electrico_averiado, [asientos_no_se_ajustan, ventanas_no_se_abren]).
relacion(control_remoto_fallido, [aire_acondicionado_no_funciona]).
relacion(conductos_obstruidos, [calefacción_inadecuada]).
relacion(sensores_fallidos, [aire_acondicionado_no_funciona, calefacción_inadecuada]).
