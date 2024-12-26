% Predicat principal que inicia el men´u.
menu :-
    write('========================'), nl,
    write(' Men´u Interactiu '), nl,
    write('========================'), nl,
    write('1. Opci´o 1'), nl,
    write('2. Opci´o 2'), nl,
    write('3. Opci´o 3'), nl,
    write('4. Opci´o 4'), nl,
    write('5. Opci´o 5'), nl,
    write('0. Sortir'), nl,
    write('========================'), nl,
    write('Escull una opcio: '),
    read(Opcio),
    processar_opcio(Opcio).

% Predicat per processar les opcions.
processar_opcio(0) :-
    write('Sessio finalitzada. Adeu!'), nl.

processar_opcio(1) :-
    write('Opcio Escollida: 1'), nl, nl, menu.

processar_opcio(2) :-
    write('Opcio Escollida: 2'), nl, nl, menu.

processar_opcio(3) :-
    write('Opcio Escollida: 3'), nl, nl, menu.

processar_opcio(4) :-
    write('Opcio Escollida: 4'), nl, nl, menu.

processar_opcio(5) :-
    write('Opcio Escollida: 5'), nl, nl, menu.

processar_opcio(_) :-
    write('Opcio no vàlida. Torna a provar.'), nl, nl, menu.