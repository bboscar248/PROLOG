:- discontiguous averia/2.
:- discontiguous causa/2.
:- discontiguous relacion/2.

% Domini de les cuines
% Tenim 3 subsistemes: 
% horno, fogones, refrigerador

% Subsistemes en bon funcionament
subsistema(cocina, horno).
subsistema(cocina, fogones).
subsistema(cocina, refrigerador).

% Funcionamento normal del sistema
% Relacion entre componentes y comportamientos esperados

% Horno
% Avarias/efectos/observaciones del subsistema de horno
averia(horno, no_calienta).
averia(horno, calienta_demasiado).
averia(horno, hace_ruido).

% Causas posibles de fallos del horno
causa(horno, resistencia_quemada).
causa(horno, termostato_fallido).
causa(horno, ventilador_roto).

% Reglas que relacionan causas y efectos del subsistema de horno
relacion(resistencia_quemada, [no_calienta]).
relacion(termostato_fallido, [calienta_demasiado]).
relacion(ventilador_roto, [hace_ruido]).

% Fogones
% Avarias/efectos/observaciones del subsistema de fogones
averia(fogones, no_encieden).
averia(fogones, llama_baja).
averia(fogones, fuga_de_gas).

% Causas posibles de fallos de los fogones
causa(fogones, chispero_roto).
causa(fogones, regulador_de_gas_fallido).
causa(fogones, manguera_rota).

% Reglas que relacionan causas y efectos del subsistema de fogones
relacion(chispero_roto, [no_encieden]).
relacion(regulador_de_gas_fallido, [llama_baja]).
relacion(manguera_rota, [fuga_de_gas]).

% Refrigerador
% Avarias/efectos/observaciones del subsistema de refrigerador
averia(refrigerador, no_enfria).
averia(refrigerador, hace_ruido).
averia(refrigerador, fuga_de_agua).

% Causas posibles de fallos del refrigerador
causa(refrigerador, compresor_roto).
causa(refrigerador, ventilador_roto).
causa(refrigerador, manguera_rota).

% Reglas que relacionan causas y efectos del subsistema de refrigerador
relacion(compresor_roto, [no_enfria]).
relacion(ventilador_roto, [hace_ruido]).
relacion(manguera_rota, [fuga_de_agua]).