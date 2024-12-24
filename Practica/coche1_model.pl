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
