% Observaciones y subsistemas
observacion(cocina, no_funciona_horno, horno).
observacion(cocina, refrigerador_no_enfria, refrigerador).
observacion(cocina, placa_no_calienta, placa_de_cocina).

% Causas para subsistemas
causa(cocina, horno, fusible_quemado).
causa(cocina, horno, termostato_averiado).
causa(cocina, refrigerador, falta_gas_refrigerante).
causa(cocina, refrigerador, motor_averiado).
causa(cocina, placa_de_cocina, termostato_averiado).
