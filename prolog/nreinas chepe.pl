% ------------------------------------

% INSTITUTO TECNOLÓGICO DE COSTA RICA.
% IC-4700 LENGUJES DE PROGRAMACIÓN.
% III PROYECTO PROGRAMADO.
% I SEMESTRE 2014.

% JOSÉ ANDRÉS GARCÍA SÁENZ.
% LEONARDO MADRIGAL VALVERDE.

% ------------------------------------

% nreinas(N, Solucion).
% Obtiene todas las soluciones al problema de las N Reinas.
% N : Tamaño del tablero.
% Solucion: Donde se guardará cada solución.

nreinas(N, Solucion) :-
	generarTablero(N, Tablero),
  nreinas(N, Tablero, [], Resultado),
  convertirTablero(N, Resultado, Solucion).

nreinas(0, _Tablero, Solucion, Solucion).

nreinas(Fila, Tablero, Reinas, Solucion) :-
    quitar(Tablero, Columna, Tablero1),
    noChocan(Fila, Columna, Reinas),
    Fila1 is Fila-1,
    nreinas(Fila1, Tablero1, [[Fila,Columna]|Reinas], Solucion).

% generarTablero(N, Tablero).
% Genera un tablero de tamaño N.
% N : Tamaño del tablero a generar.
% Tablero: Donde se guardará el tablero generado.

generarTablero(0, []) :- !.

generarTablero(N, [N|T]) :-
	N1 is N-1,
	generarTablero(N1, T).

% choca(Fila1, Columna1, Fila2, Columna2).
% Comprueba si una reina se ataca con otra.
% Fila1 : Fila de la primera reina.
% Columna1 : Columna de la primera reina.
% Fila2 : Fila de la segunda reina.
% Columna2 : Columna de la segunda reina.

choca(F1, C1, F2, C2) :-
    Sum is F1+C1,
    Sum is F2+C2.
    
choca(F1, C1, F2, C2) :-
    Sub is F1-C1,
    Sub is F2-C2.

% noChocan(Fila, Columna, Tablero).
% Comprueba si dado un tablero ninguna de sus reinas se ataca entre si.
% Fila : Fila actual.
% Columna : Columna actual.
% Tablero : El tablero a comprobar.

noChocan(_Fila, _Columna, []).

noChocan(Fila, Columna, [[F,C]|Resto]) :-
    \+ choca(Fila, Columna, F, C),
    noChocan(Fila, Columna, Resto).

% quitar(Lista, Elemento, Resultado).
% Elimina un elemento de una lista.
% Lista : Lista original.
% Elemento : Elemento a eliminar.
% Resultado : En donde se guardará la lista sin el elemento.

quitar([H|T], H, T).

quitar([H1|T1], X, [H1|T2]) :- quitar(T1, X, T2).

% convertirTablero(N, Tablero, Salida).
% Convierte un tablero a su representación gráfica.
% N : Tamaño del tablero.
% Tablero : El tablero a convertir.
% Salida : Donde se guardará la representación gráfica del tablero.

convertirTablero(_N, [], []) :- !.

convertirTablero(N, [Fila|RestoTablero], [Salida|RestoSalida]) :-
	convertirFila(N, Fila, Salida),
	convertirTablero(N, RestoTablero, RestoSalida).

% convertirFila(N, Fila, Salida).
% Convierte una fila a su representación gráfica.
% N : Tamaño de la fila.
% Fila : La fila a convertir.
% Salida : Donde se guardará la representación gráfica de la fila.

convertirFila(N, Fila, Salida) :- convertirFila(Fila, N, 0, Salida), !.

convertirFila(_Fila, N, N, '') :- !.

convertirFila([F,C], N, C, Salida) :-
	Contador1 is C+1,
	convertirFila([F,C], N, Contador1, Resto),
	atom_concat('Q', Resto, Salida).

convertirFila([F,C], N, Contador, Salida) :-
	Contador1 is Contador+1,
	convertirFila([F,C], N, Contador1, Resto),
	atom_concat('_', Resto, Salida).
