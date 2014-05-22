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
	permutation(Tablero, TableroPermutado),
	noChocan(TableroPermutado),
	convertirTablero(N, TableroPermutado, Solucion).

% generarTablero(N, Tablero).
% Genera un tablero de tamaño N.
% N : Tamaño del tablero a generar.
% Tablero: Donde se guardará el tablero generado.

generarTablero(0, []) :- !.

generarTablero(N, [H|T]) :-
	H is N - 1,
	generarTablero(H, T).

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

convertirFila(N, Fila, Salida) :- convertirFila(N, Fila, 0, Salida), !.

convertirFila(N, _Fila, N, []) :- !.

convertirFila(N, Fila, Fila, ['Q'|Resto]) :-
	Contador is Fila + 1,
	convertirFila(N, Fila, Contador, Resto).

convertirFila(N, Fila, Contador, ['_'|Resto]) :-
	Contador1 is Contador + 1,
	convertirFila(N, Fila, Contador1, Resto).

% noChocan(Tablero).
% Comprueba si dado un tablero ninguna de sus reinas se ataca entre si.
% Tablero : El tablero a comprobar.

noChocan([]) :- !.

noChocan([Fila|RestoTablero]) :-
	\+ choca(Fila, RestoTablero),
	noChocan(RestoTablero).

% choca(Fila, Tablero).
% Comprueba si una reina se ataca con alguna de las demás reinas del tablero.
% Reina : Reina a comprobar.
% Tablero : El tablero donde se encuentran las demás reinas.

choca(Reina, Tablero) :- choca(Reina, 1, Tablero), !.

choca(Reina, Profundidad, [Fila|_RestoTablero]) :-
	Reina =:= Fila + Profundidad;
	Reina =:= Fila - Profundidad;
	Reina =:= Fila.

choca(Reina, Profundidad, [_Fila|RestoTablero]) :-
	Profundidad1 is Profundidad + 1,
	choca(Reina, Profundidad1, RestoTablero).
