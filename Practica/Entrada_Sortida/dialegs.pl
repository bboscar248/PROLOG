% Predicat per demanar el nom i saludar l'usuari. 
salutacio :- 
    write('Quin és el teu nom?'), 
    read(Nom), 
    format('Hola, ~w! Encantat de conèixer-te. ~n', [Nom]). 