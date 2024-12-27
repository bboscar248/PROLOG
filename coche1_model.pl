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
relacion(correa_de_la_direccion_floja, [direccion_dura]).
relacion(falta_de_lubricación_en_el_sistema_de_rótulas, [ruido_al_girar]).
relacion(falta_de_lubricación_en_el_sistema_de_cremallera, [ruido_al_girar]).
relacion(desgaste_de_neumaticos, [vibraciones_volante]).
relacion(suspension_danada, [vibraciones_volante, desviacion_hacia_un_lado]).
relacion(deterioro_de_los_silentblocks, [vibraciones_volante]).
relacion(problemas_de_presion_neumaticos, [desviacion_hacia_un_lado]).
relacion(problemas_barra_de_direccion, [desviacion_hacia_un_lado, coche_no_gira]).
relacion(rotura_de_piezas, [coche_no_gira]).




% Transmissión 


% Encendido


% Sistema electrico


% Sistema de confort interior
% Avarias/efectos/observaciones del subsistema de confort interior
averia(sistema_confort_interior, fallos_en_el_sistema_de_climatización).
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
relacion(fugas_de_refrigerante, [fallos_en_el_sistema_de_climatización]). 
relacion(compresor_defectuoso, [fallos_en_el_sistema_de_climatización]).
relacion(sensor_de_temperatura_defectuoso, [fallos_en_el_sistema_de_climatización]).
relacion(obstruccion_en_las_salidas_de_aire, [fallos_en_el_sistema_de_climatización]).
relacion(fusible_fundido, [fallos_en_el_sistema_de_climatización, problemas_en_el_sistema_de_audio, problema_para_subir_y_bajar_las_ventanas, no_se_ajustan_los_asientos, no_funciona_el_cierre_centralizado, no_funciona_las_luces_interiores]).
relacion(problema_de_cables, [problemas_en_el_sistema_de_audio, no_funciona_las_luces_interiores, problema_para_subir_y_bajar_las_ventanas, no_funciona_el_cierre_centralizado]).
relacion(amplificador_roto, [problemas_en_el_sistema_de_audio]).
relacion(rotura_del_mecanismo_del_elevalunas, [problema_para_subir_y_bajar_las_ventanas]).
relacion(acumulacion_de_suciedad, [problema_para_subir_y_bajar_las_ventanas]).
relacion(no_funciona_el_motor_de_elevacion, [problema_para_subir_y_bajar_las_ventanas]).
relacion(palanca_del_asiento_danada, [no_se_ajustan_los_asientos]).
relacion(piezas_danadas, [fallos_en_el_sistema_de_climatización, problemas_en_el_sistema_de_audio, problema_para_subir_y_bajar_las_ventanas, no_se_ajustan_los_asientos, no_funciona_el_cierre_centralizado, no_funciona_las_luces_interiores]).
relacion(motor_electrico_averiado, [no_se_ajustan_los_asientos, problema_para_subir_y_bajar_las_ventanas, no_funciona_el_cierre_centralizado]).
relacion(problema_con_el_solenoide, [no_funciona_el_cierre_centralizado]).