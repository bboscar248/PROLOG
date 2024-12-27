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
averia(direccion, ruido_al_girar).
averia(direccion, direccion_dura).
averia(direccion, coche_no_gira).
averia(direccion, desviacion_hacia_un_lado).



% Causas posibles de fallos del coche del subsistema de dirección
causa(direccion, falta_de_liquido_de_direccion).
causa(direccion, fallos_en_la_bomba_de_direccion_asistida). 
causa(direccion, fallo_en_las_rotulas). 
causa(direccion, fallo_en_los_bujes).
causa(direccion, desgastos_en_los_brazos_de_control).
causa(direccion, presion_no_adecuada). 
causa(direccion, correa_de_la_direccion_floja).
causa(direccion, falta_de_lubricación_en_el_sistema_de_rótulas). 
causa(direccion, falta_de_lubricación_en_el_sistema_de_cremallera). 
causa(direccion, ruedas_mal_alineadas).
causa(direccion, desgaste_de_neumaticos). 
causa(direccion, problemas_de_presion_neumaticos). 
causa(direccion, suspension_danada). 
causa(direccion, deterioro_de_los_silentblocks). 
causa(direccion, problemas_barra_de_direccion).
causa(direccion, rotura_de_piezas).




% Reglas que relacionan causas y efectos del subsistema de dirección
relacion(falta_de_liquido_de_direccion, [direccion_deficiente, direccion_dura]). 
relacion(fallos_en_la_bomba_de_direccion_asistida, [direccion_deficiente, direccion_dura]).
relacion(ruedas_mal_alineadas, [direccion, deficiente, vibraciones_volante, desviación_hacia_un_lado]).
relacion(fallo_en_las_rotulas, [direccion_deficiente, ruido_al_girar]).
relacion(fallo_en_los_bujes, [direccion_deficiente, ruido_al_girar]).
relacion(desgastos_en_los_brazos_de_control, [direccion_deficiente]).
relacion(presion_no_adecuada, [direccion_dura]).
relacion(correa_de_la_floja, [direccion_dura]).
relacion(falta_de_lubricación_en_el_sistema_de_rótulas, [ruido_al_girar]).
relacion(falta_de_lubricación_en_el_sistema_de_cremallera, [ruido_al_girar]).
relacion(desgaste_de_neumaticos, [vibraciones_volante]).
relacion(suspension_danada, [vibraciones_volante, desviacion_hacia_un_lado]).
relacion(deterioro_de_los_silentblocks, [vibraciones_volante]).
relacion(problemas_de_presion_neumaticos, [desviacion_hacia_un_lado]).
relacion(problemas_barra_de_direccion, [desviacion_hacia_un_lado, coche_no_gira]).
relacion(rotura_de_piezas, [coche_no_gira]).


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
