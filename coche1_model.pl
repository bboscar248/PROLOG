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





% Transmissión 


% Encesa


% Sistema elèctric


% Sistema de confort interior