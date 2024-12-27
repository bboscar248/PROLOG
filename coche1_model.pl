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
averia(direccion, ruido_al_girar).
averia(direccion, direccion_dura).
averia(direccion, direccion_deficiente). 
averia(direccion, direccion_flacida).
averia(direccion, el_coche_no_gira).
averia(direccion, desviación_hacia_un_lado).

% Causas posibles de fallos del coche del subsistema de dirección
causa(direccion, falta_de_liquido_de_direccion).
causa(direccion, fallos_en_la_bomba_de_direccion_asistida). 
causa(direccion, desalineacion_de_las_ruedas).
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





% Reglas que relacionan causas y efectos del subsistema de dirección
relacion(falta_de_liquido_de_direccion, [direccion_deficiente, direccion_dura]). 
relacion(fallos_en_la_bomba_de_direccion_asistida, [direccion_deficiente, direccion_dura]).
relacion(desalineacion_de_las_ruedas, [direccion_deficiente]).
relacion(fallo_en_las_rotulas, [direccion_deficiente, ruido_al_girar]).
relacion(fallo_en_los_bujes, [direccion_deficiente, ruido_al_girar]).
relacion(desgastos_en_los_brazos_de_control, [direccion_deficiente]).
relacion(presion_no_adecuada, [direccion_dura]).
relacion(correa_de_la_floja, [direccion_dura]).
relacion(falta_de_lubricación_en_el_sistema_de_rótulas, [ruido_al_girar]).
relacion(falta_de_lubricación_en_el_sistema_de_cremallera, [ruido_al_girar]).
relacion(ruedas_mal_alineadas, [vibraciones_volante, desviación_hacia_un_lado]).
relacion(desgaste_de_neumaticos, [vibraciones_volante]).
relacion(suspension_danada, [vibraciones_volante]).
relacions(deterioro_de_los_silentblocks, [vibraciones_volante]).










relacion(falta_de_fluido_hidraulico, [volante_vibrante, coche_no_gira, coche_gira_solo]).
relacion(bomba_hidraulica_averiada, [coche_no_gira]).
relacion(desgaste_mangueras_hidraulicas, [coche_no_gira]).
relacion(deterioro_de_las_juntas_y_sellos, [coche_no_gira]).
relacion(desgaste_de_neumaticos, [volante_vibrante]).
relacion(caja_de_direccion_danada, [coche_gira_solo]).
relacion(barra_de_direccion_danada, [coche_gira_solo]). 
relacion(erosion_de_soportes, [volante_desalineado]).










% Transmissión 


% Encesa


% Sistema elèctric


% Sistema de confort interior