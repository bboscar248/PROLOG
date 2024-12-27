% Observaciones y subsistemas
observacion(coche, no_funciona_aire_acondicionado, aire_acondicionado).
observacion(coche, no_funciona_luz_delantera, sistema_electrico).
observacion(coche, frenos_no_responden, frenos).
observacion(coche, pierde_aceite, motor).

% Causas para subsistemas
causa(coche, aire_acondicionado, falta_gas_refrigerante).
causa(coche, aire_acondicionado, compresor_averiado).
causa(coche, sistema_electrico, fusible_quemado).
causa(coche, sistema_electrico, interruptor_averiado).
causa(coche, frenos, pastillas_de_freno_danadas).
causa(coche, frenos, liquido_de_freno_bajo).
causa(coche, motor, aceite_bajo).
causa(coche, motor, fallo_en_bujias).




% Domini dels cotxes
% Tenim 5 subsistemes: 
% direcció, transmissió, encesa, sistema elèctric, sistema de confort interior

% Subsistemes en bon funcionament
subsistema(coche, direccio).
subsistema(coche, transmissio).
subsistema(coche, encesa).
subsistema(coche, sistema_electric).
subsistema(coche, sistema_confort_interior).


% Funcionamento normal del sistema
% Relación entre componentes y comportamientos esperados

% Dirección
% Avarias/efectos/observaciones del subsistema de dirección

avaria(direccio, volant_vibrant).
avaria(direccio, cotxe_no_gira).
avaria(direccio, cotxe_gira_sol).
avaria(direccio, volant_desalineat).
avaria(direccio, cotxe_no_gira).

% Causas posibles de fallos del coche del subsistema de dirección
causa(direccio, falta_de_fluid_hidraulic). 
causa(direccio, bomba_hidraulica_averiada).
causa(direccio, desgast_de_pneumatics). 
causa(direccio, caixa_de_direccio_danada). 
causa(direccio, barra_de_direccio_danada).  


% Reglas que relacionan causas y efectos del subsistema de dirección
...






% Transmissión 


% Encesa


% Sistema elèctric


% Sistema de confort interior