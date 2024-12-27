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




% Transmissión 


% Encendido


% Sistema electrico


% Sistema de confort interior