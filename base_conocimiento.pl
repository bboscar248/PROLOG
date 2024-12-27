% Domini dels cotxes
% Tenim 5 subsistemes: 
% direccio, transmissio, encesa, sistema elèctric, sistema de confort interior

% Subsistemes en bon funcionament
subsistema(coche, direccion).
subsistema(coche, transmission).
subsistema(coche, encesa).
subsistema(coche, sistema_electrico).
subsistema(coche, sistema_confort_interior).


% Funcionamento normal del sistema
% Relacion entre componentes y comportamientos esperados

% Direccion
% Avarias/efectos/observaciones del subsistema de direccion

averia(direccion, vibraciones_volante).
averia(direccion, ruido_al_girar).
averia(direccion, direccion_dura).
averia(direccion, coche_no_gira).
averia(direccion, desviacion_hacia_un_lado).

% Causas posibles de fallos del coche del subsistema de direccion
causa(direccion, falta_de_liquido_de_direccion).
causa(direccion, fallos_en_la_bomba_de_direccion_asistida). 
causa(direccion, fallo_en_las_rotulas). 
causa(direccion, fallo_en_los_bujes).
causa(direccion, desgastos_en_los_brazos_de_control).
causa(direccion, presion_no_adecuada). 
causa(direccion, correa_de_la_direccion_floja).
causa(direccion, falta_de_lubricacion_en_el_sistema_de_rotulas). 
causa(direccion, falta_de_lubricacion_en_el_sistema_de_cremallera). 
causa(direccion, ruedas_mal_alineadas).
causa(direccion, desgaste_de_neumaticos). 
causa(direccion, problemas_de_presion_neumaticos). 
causa(direccion, suspension_danada). 
causa(direccion, deterioro_de_los_silentblocks). 
causa(direccion, problemas_barra_de_direccion).
causa(direccion, rotura_de_piezas).

% Reglas que relacionan causas y efectos del subsistema de direccion
relacion(falta_de_liquido_de_direccion, [direccion_deficiente, direccion_dura]). 
relacion(fallos_en_la_bomba_de_direccion_asistida, [direccion_deficiente, direccion_dura]).
relacion(ruedas_mal_alineadas, [direccion, deficiente, vibraciones_volante, desviacion_hacia_un_lado]).
relacion(fallo_en_las_rotulas, [direccion_deficiente, ruido_al_girar]).
relacion(fallo_en_los_bujes, [direccion_deficiente, ruido_al_girar]).
relacion(desgastos_en_los_brazos_de_control, [direccion_deficiente]).
relacion(presion_no_adecuada, [direccion_dura]).
relacion(correa_de_la_direccion_floja, [direccion_dura]).
relacion(falta_de_lubricacion_en_el_sistema_de_rotulas, [ruido_al_girar]).
relacion(falta_de_lubricacion_en_el_sistema_de_cremallera, [ruido_al_girar]).
relacion(desgaste_de_neumaticos, [vibraciones_volante]).
relacion(suspension_danada, [vibraciones_volante, desviacion_hacia_un_lado]).
relacion(deterioro_de_los_silentblocks, [vibraciones_volante]).
relacion(problemas_de_presion_neumaticos, [desviacion_hacia_un_lado]).
relacion(problemas_barra_de_direccion, [desviacion_hacia_un_lado, coche_no_gira]).
relacion(rotura_de_piezas, [coche_no_gira]).



% Transmissio

% Avarias/efectos/observaciones del subsistema de transmisión
averia(transmission, dificultad_para_cambiar_marchas).
averia(transmission, perdida_de_potencia).
averia(transmission, ruidos_extraños_en_la_caja).
averia(transmission, vibraciones_al_conducir).
averia(transmission, sobrecalentamiento).

% Causas posibles de fallos del coche del subsistema de transmisión
causa(transmission, nivel_bajo_de_aceite_transmision).
causa(transmission, desgaste_sincronizadores).
causa(transmission, embrague_desgastado).
causa(transmission, filtro_obstruido).
causa(transmission, fallo_en_la_bomba_de_aceite).
causa(transmission, convertidor_de_par_defectuoso).
causa(transmission, engranajes_desgastados).
causa(transmission, soportes_del_motor_desgastados).

% Reglas que relacionan causas y efectos del subsistema de transmisión
relacion(nivel_bajo_de_aceite_transmision, [dificultad_para_cambiar_marchas, sobrecalentamiento]).
relacion(desgaste_sincronizadores, [dificultad_para_cambiar_marchas]).
relacion(embrague_desgastado, [perdida_de_potencia]).
relacion(filtro_obstruido, [sobrecalentamiento]).
relacion(fallo_en_la_bomba_de_aceite, [ruidos_extraños_en_la_caja]).
relacion(convertidor_de_par_defectuoso, [perdida_de_potencia]).
relacion(engranajes_desgastados, [ruidos_extraños_en_la_caja]).
relacion(soportes_del_motor_desgastados, [vibraciones_al_conducir]).




% Encesa

% Avarias/efectos/observaciones del subsistema de encendido
averia(encesa, el_motor_no_arranca).
averia(encesa, ralentí_irregular).
averia(encesa, consumo_excesivo_combustible).
averia(encesa, detonaciones).
averia(encesa, perdida_de_potencia).
averia(encesa, dificultad_arranque_en_frio).
averia(encesa, chispa_debil).

% Causas posibles de fallos del coche del subsistema de encendido
causa(encesa, bujias_desgastadas).
causa(encesa, cables_de_bujias_danados).
causa(encesa, bobina_de_encendido_fallida).
causa(encesa, fallo_del_sensor_de_cigueñal).
causa(encesa, distribuidor_defectuoso).
causa(encesa, mezcla_combustible_pobre).
causa(encesa, bateria_descargada).
causa(encesa, regulador_de_voltaje_defectuoso).

% Reglas que relacionan causas y efectos del subsistema de encendido
relacion(bujias_desgastadas, [el_motor_no_arranca, ralentí_irregular, chispa_debil]).
relacion(cables_de_bujias_danados, [ralentí_irregular, perdida_de_potencia]).
relacion(bobina_de_encendido_fallida, [el_motor_no_arranca, detonaciones, chispa_debil]).
relacion(fallo_del_sensor_de_cigueñal, [el_motor_no_arranca]).
relacion(distribuidor_defectuoso, [detonaciones, ralentí_irregular]).
relacion(mezcla_combustible_pobre, [consumo_excesivo_combustible, perdida_de_potencia]).
relacion(bateria_descargada, [dificultad_arranque_en_frio]).
relacion(regulador_de_voltaje_defectuoso, [chispa_debil]).

% Sistema elèctric
% Avarias/efectos/observaciones del subsistema eléctrico
averia(sistema_electrico, luces_no_funcionan).
averia(sistema_electrico, bateria_descargada).
averia(sistema_electrico, alternador_fallido).
averia(sistema_electrico, cortocircuitos).
averia(sistema_electrico, fusibles_fundidos).
averia(sistema_electrico, sistema_no_responde).
averia(sistema_electrico, sobrecalentamiento_cables).

% Causas posibles de fallos del coche del subsistema eléctrico
causa(sistema_electrico, cableado_danado).
causa(sistema_electrico, bateria_anticuada).
causa(sistema_electrico, regulador_de_voltaje_defectuoso).
causa(sistema_electrico, conexiones_sueltas).
causa(sistema_electrico, alternador_dañado).
causa(sistema_electrico, cortocircuito).

% Reglas que relacionan causas y efectos del subsistema eléctrico
relacion(cableado_danado, [cortocircuitos, luces_no_funcionan, sobrecalentamiento_cables]).
relacion(bateria_anticuada, [bateria_descargada, sistema_no_responde]).
relacion(regulador_de_voltaje_defectuoso, [alternador_fallido, sistema_no_responde]).
relacion(conexiones_sueltas, [fusibles_fundidos]).
relacion(alternador_dañado, [bateria_descargada, alternador_fallido]).
relacion(cortocircuito, [cortocircuitos, sobrecalentamiento_cables]).




% Sistema de confort interior
% Avarias/efectos/observaciones del subsistema de confort interior
averia(sistema_confort_interior, fallos_en_el_sistema_de_climatizacion).
averia(sistema_confort_interior, problemas_en_el_sistema_de_audio).
averia(sistema_confort_interior, problema_para_subir_y_bajar_las_ventanas).
averia(sistema_confort_interior, no_se_ajustan_los_asientos).
averia(sistema_confort_interior, no_funciona_el_cierre_centralizado).
averia(sistema_confort_interior, no_funciona_las_luces_interiores). 

% Causas posibles de fallos del coche del subsistema de confort interior
causa(sistema_confort_interior, fugas_de_refrigerante). 
causa(sistema_confort_interior, compresor_defectuoso).
causa(sistema_confort_interior, sensor_de_temperatura_defectuoso).
causa(sistema_confort_interior, obstruccion_en_las_salidas_de_aire). 
causa(sistema_confort_interior, fusible_fundido).
causa(sistema_confort_interior, problema_de_cables).
causa(sistema_confort_interior, amplificador_roto).
causa(sistema_confort_interior, rotura_del_mecanismo_del_elevalunas).
causa(sistema_confort_interior, acumulacion_de_suciedad).
causa(sistema_confort_interior, no_funciona_el_motor_de_elevacion).
causa(sistema_confort_interior, palanca_del_asiento_danada). 
causa(sistema_confort_interior, piezas_danadas). 
causa(sistema_confort_interior, motor_electrico_averiado).
causa(sistema_confort_interior, problema_con_el_solenoide).

% Reglas que relacionan causas y efectos del subsistema de confort interior
relacion(fugas_de_refrigerante, [fallos_en_el_sistema_de_climatizacion]). 
relacion(compresor_defectuoso, [fallos_en_el_sistema_de_climatizacion]).
relacion(sensor_de_temperatura_defectuoso, [fallos_en_el_sistema_de_climatizacion]).
relacion(obstruccion_en_las_salidas_de_aire, [fallos_en_el_sistema_de_climatizacion]).
relacion(fusible_fundido, [fallos_en_el_sistema_de_climatizacion, problemas_en_el_sistema_de_audio, problema_para_subir_y_bajar_las_ventanas, no_se_ajustan_los_asientos, no_funciona_el_cierre_centralizado, no_funciona_las_luces_interiores]).
relacion(problema_de_cables, [problemas_en_el_sistema_de_audio, no_funciona_las_luces_interiores, problema_para_subir_y_bajar_las_ventanas, no_funciona_el_cierre_centralizado]).
relacion(amplificador_roto, [problemas_en_el_sistema_de_audio]).
relacion(rotura_del_mecanismo_del_elevalunas, [problema_para_subir_y_bajar_las_ventanas]).
relacion(acumulacion_de_suciedad, [problema_para_subir_y_bajar_las_ventanas]).
relacion(no_funciona_el_motor_de_elevacion, [problema_para_subir_y_bajar_las_ventanas]).
relacion(palanca_del_asiento_danada, [no_se_ajustan_los_asientos]).
relacion(piezas_danadas, [fallos_en_el_sistema_de_climatizacion, problemas_en_el_sistema_de_audio, problema_para_subir_y_bajar_las_ventanas, no_se_ajustan_los_asientos, no_funciona_el_cierre_centralizado, no_funciona_las_luces_interiores]).
relacion(motor_electrico_averiado, [no_se_ajustan_los_asientos, problema_para_subir_y_bajar_las_ventanas, no_funciona_el_cierre_centralizado]).
relacion(problema_con_el_solenoide, [no_funciona_el_cierre_centralizado]).