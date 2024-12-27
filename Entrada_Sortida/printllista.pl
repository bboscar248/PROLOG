% Regla principal que imprime la lista
printllista([]) :- 
    write([]).     % Si la lista está vacía, imprime directamente []

printllista([Cabeza|Cola]) :-
    write('['), 
    print_elementos([Cabeza|Cola]). 

% Si la lista solo tiene un elemento
print_elementos([Cabeza]) :-
    write(Cabeza), 
    write(']'). 

print_elementos([Cabeza|Cola]) :- 
    write(Cabeza),      % Imprime el primer elemento de la lista
    write(','),         % Imprime una coma después del elemento
    print_elementos(Cola).  % Llama recursivamente a la función para la cola de la lista