% Reglas para el coche

% Causas -> Avería
causa(no_funciona_aire_acondicionado, aire_acondicionado).
causa(no_funciona_luz_delantera, sistema_electrico).
causa(frenos_no_responden, frenos).
causa(no_arranca, motor).
causa(ruido_extrano_motor, motor).
causa(vehiculo_vibrante, suspension).
causa(pierde_aceite, motor).
causa(frenos_chirrian, frenos).
causa(vehiculo_sobreaquema, sistema_de_refri).
causa(agua_en_asientos, sistema_de_aceleracion).
causa(escapes_fuertes, sistema_de_escape).
causa(sonido_bajo_aire_acondicionado, aire_acondicionado).
causa(volante_dificil, direccion).
causa(bateria_baja, electrico).
causa(cambio_tiemblos, suspension).

% Efectos -> Causas posibles
efecto(aire_acondicionado, [falta_gas_refrigerante, compresor_averiado, filtro_sucio]).
efecto(sistema_electrico, [fusible_quemado, interruptor_averiado, bateria_descargada]).
efecto(frenos, [pastillas_de_freno_danadas, liquido_de_freno_bajo, disco_de_freno_danado]).
efecto(motor, [filtro_de_aire_sucio, bujias_averiadas, correa_de_timing_rota, bateria_descargada]).
efecto(suspension, [amortiguadores_danados, resortes_roto, barra_de_suspension_danada]).
efecto(sistema_de_refri, [termostato_averiado, radiador_bloqueado, liquido_de_refrigeracion_bajo]).
efecto(sistema_de_aceleracion, [cable_de_aceleracion_roto, bomba_de_gasolina_averiada]).
efecto(sistema_de_escape, [catalizador_averiado, silencioso_danado]).
efecto(direccion, [bomba_de_direccion_hidraulica_averiada, correa_de_direccion_rota]).
efecto(electrico, [fusible_quemado, alternador_danado]).
efecto(suspension, [rodamientos_danados, problemas_con_el_eje]).