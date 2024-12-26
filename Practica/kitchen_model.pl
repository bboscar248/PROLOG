% Reglas para la cocina

% Causas -> Avería
causa(no_funciona_horno, horno).
causa(refrigerador_no_enfria, refrigerador).
causa(placa_no_calienta, placa_de_cocina).
causa(agua_no_caliente, calentador_agua).
causa(ventilador_no_funciona, horno).
causa(fugas_de_agua, lavadora).
causa(agua_en_el_suelo, lavavajillas).
causa(olores_raros_en_el_refrigerador, refrigerador).
causa(ruido_extrano_lavadora, lavadora).
causa(tiempos_lavado_excesivos, lavadora).
causa(temperatura_agua_irregular, calentador_agua).
causa(ventilador_no_enfriar, refrigerador).
causa(puerta_oven_no_cierra, horno).
causa(salida_de_agua_lavadora, lavadora).
causa(sonido_estruendoso_lavavajillas, lavavajillas).

% Efectos -> Causas posibles
efecto(horno, [fusible_quemado, termostato_averiado, interruptor_averiado, resistencia_rotas, ventilador_averiado]).
efecto(refrigerador, [falta_gas_refrigerante, motor_averiado, termostato_averiado, compresor_averiado, ventilador_averiado]).
efecto(placa_de_cocina, [fusible_quemado, interruptor_averiado, cable_rotos, quemadores_averiados]).
efecto(calentador_agua, [termostato_averiado, resistencias_averiadas, cableado_danado]).
efecto(lavadora, [motor_averiado, manguera_de_drenaje_bloqueada, filtro_bloqueado, correa_rota]).
efecto(lavavajillas, [motor_averiado, bomba_de_drenaje_bloqueada, termostato_averiado, manguera_bloqueada]).
