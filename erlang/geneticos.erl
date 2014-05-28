-module(geneticos).
-import(lists,[append/2, split/2]).
-import(string,[concat/2]).
-export([generarPoblacion/2, chocaColumna/3, cant_choca/2,  chocaDiagonalDerecha/3, chocaDiagonalIzquierda/3, getAt/2, mejorDePoblacion/1, seleccion/2, eliminarDePoblacion/2]). 
-export([fitness/1, shuffle/1, corte/2, cruce/2, cruce/1, agregarCruce/2, crearNuevaPoblacion/4, mutar/2, crearFila/2, printIndividuo/1, imprimirPoblacion/1]).
-export([solucion/1, nreinas/4, generarTabla/1, sumaFitness/1, generarAcumulada/1, seleccionarDeTabla/3]).
%Obtiene el elemento de una lista dada una posición.
%Recibe: L = Lista de elementos.
%		 N = Posición de la lista.
%Retorna: Elemento en la posición indicada.	
getAt([], _N)-> [];
getAt(L, N)-> getAt(L, N, 1).
getAt([H|_T], N, N)-> H;
getAt([_H|T], N, C)-> getAt(T, N, C+1).

shuffle(L) -> shuffle(L, []).
shuffle([], Acc) -> Acc;
shuffle(L, Acc) -> {Leading, [H|T]} = lists:split(random:uniform(length(L))-1, L), shuffle(Leading ++ T, [H|Acc]).

iota(N) -> iota(N, 1).
iota(N, N) -> [N];
iota(N, C) -> [C] ++ iota(N, C+1).


%Crea la población inicial.
% Recibe: N = Tamaño del tablero.
%		  T = Tamaño de la población.
generarPoblacion(N, T) -> generarPoblacion(N, T, 0).

% Recibe: N = Cantidad de individuos a generar.
%		  T = Tamaño de la poplación.
%		  C = Contador de individuos.
%		  M = Lista que contiene los individuos(Listas de posiciones).
%Retorna: Matriz o lista de individuos creados.
generarPoblacion(_N, T, T) -> [];
generarPoblacion(N, T, C) -> [crearIndividuo(N)] ++ generarPoblacion(N, T, C+1).

%------------------------------------Funciones para impresión de la población---------------------------------------------------------------------
crearFila(Pos, N)-> crearFila(Pos, N+1, 1, "").
crearFila(_Pos, N, N, Res)-> [Res];
crearFila(Pos, N, Pos, Res)-> crearFila(Pos, N, Pos+1, concat(Res, " Q "));
crearFila(Pos, N, C, Res)-> crearFila(Pos, N, C+1, concat(Res, " _ ")).

printIndividuo([H|T])-> printIndividuo(T, length([H|T]), [crearFila(H, length([H|T]))]).
printIndividuo([], _N, Res)-> Res;
printIndividuo([H|T], N, Res)-> printIndividuo(T, N, append(Res, [crearFila(H, N)])).

agregarEspacio(N)-> agregarEspacio(N+1, 1, "").
agregarEspacio(N, N, Res)->[Res];
agregarEspacio(N, C, Res)-> agregarEspacio(N, C+1, concat(Res," ")).

%Población a imprimir
imprimirPoblacion(P)-> imprimirPoblacion(P, length(P)+1, 1, []).
imprimirPoblacion([], N, N, Res)->Res;
imprimirPoblacion([[H|T]|REST], N, C, Res)-> imprimirPoblacion(REST, N, C+1, append(append(Res, printIndividuo([H|T])), agregarEspacio(N))).

%--------------------------------------------------------------------------------------------------------------------------------------------------

%Crea una lista con posiciones, que indican la posicion donde se encuentra la reina en la fila.
%Recibe: N = Cantidad de genes del individuo.
%Retorna: Nuevo individuo.
crearIndividuo(N) -> shuffle(iota(N)).

fitness([]) -> 0;
fitness([Reina|RestoTablero]) -> cant_choca(Reina, RestoTablero) + fitness(RestoTablero).

%Indica si una reina choca con otra en la misma columna.
%Recibe: R = Reina a revisar con los demás números del individuo.
%		 
chocaColumna(Reina, Reina, _Profundidad) -> 1;
chocaColumna(_Reina, _Tablero, _Profundidad) -> 0.

%Indica la candtidad de veces que una reina choca con otra en la diagonal izquierda.
%Recibe: Reina = Reina a revisar.
%		 N = Fila donde está la reina.
%		 Profundidad = 
chocaDiagonalIzquierda(Reina, Fila, Profundidad) when Reina == Fila - Profundidad -> 1;
chocaDiagonalIzquierda(_Reina, _Tablero, _Profundidad) -> 0.


%Indica la candtidad de veces que una reina choca con otra en la diagonal derecha.
%Recibe: Reina = Reina a revisar.
%		 N = Fila donde está la reina.
%		 Profundidad = 
chocaDiagonalDerecha(Reina, Fila, Profundidad) when Reina == Fila + Profundidad -> 1;
chocaDiagonalDerecha(_Reina, _Tablero, _Profundidad) -> 0.

%Indica la cantidad de veces que chocan las reinas en un individuo.
%Recibe: Reina = Reina a revisar.
%		 Tablero = El tablero a revisar.
%Retorna: Cnatidad de veces que choca una reina en el tablero.
cant_choca(Reina, Tablero) -> cant_choca(Reina, Tablero, 1).
cant_choca(_Reina, [], _Profundidad) -> 0;
cant_choca(Reina, [Fila|RestoTablero], Profundidad) -> chocaColumna(Reina, Fila, Profundidad)  + chocaDiagonalIzquierda(Reina, Fila, Profundidad) + chocaDiagonalDerecha(Reina, Fila, Profundidad) + cant_choca(Reina, RestoTablero, Profundidad+1).

%Función que indica el mejor individuo de una pobloación.
%Recibe: M = Matriz o lista de individuos de la población.
mejorDePoblacion([Individuo|RestoPoblacion]) -> mejorDePoblacion([Individuo|RestoPoblacion], fitness(Individuo), fitness(Individuo), Individuo).

%Función que indica el mejor individuo de una pobloación.
%Recibe: M = Matriz o lista de individuos de la población.
%		 L = Mejor individuo encontrado hasta el momento.
%		 R = fitness del mejor individuo encontrado.
%		 R2 = fitness del segundo individuo a comparar.
%		 BEST = Mejor individuo encontrado.
mejorDePoblacion([Individuo], ActualFitness, MejorFitness, _MejorIndividuo) when ActualFitness < MejorFitness -> Individuo;
mejorDePoblacion([_Individuo], _ActualFitness, _MejorFitness, MejorIndividuo) -> MejorIndividuo;
mejorDePoblacion([Individuo|[SiguienteIndividuo|RestoPoblacion]], ActualFitness, MejorFitness, _MejorIndividuo) when ActualFitness < MejorFitness -> mejorDePoblacion([SiguienteIndividuo|RestoPoblacion], fitness(SiguienteIndividuo), ActualFitness, Individuo);
mejorDePoblacion([_Individuo|[SiguienteIndividuo|RestoPoblacion]], _ActualFitness, MejorFitness, MejorIndividuo) -> mejorDePoblacion([SiguienteIndividuo|RestoPoblacion], fitness(SiguienteIndividuo), MejorFitness, MejorIndividuo).

%Función que elimina un individuo de la población.
%Recibe: P = Población.
%		 L = Individuo a eliminar.
%Retorna: La población sin el elemento.
eliminarDePoblacion([], [_H|_T])-> [];
eliminarDePoblacion([[H|T]|REST], [H|T])-> REST;
eliminarDePoblacion([[H|T]|REST], [H2|T2])-> [[H|T]|eliminarDePoblacion(REST, [H2|T2])].

%Suma el fitness de todos los individuos de una población.
sumaFitness([])->0;%Para maximizar el fitness es la cantidad de veces que choca menos el tamaño del tablero N*N, no solamente N.
sumaFitness([Individuo|Resto])-> ((length(Individuo)*length(Individuo)) - fitness(Individuo)) + sumaFitness(Resto).

%Genera una tabla acumulada de las probabilidades de cada individuo.
generarAcumulada([H|T])-> generarAcumulada(T, H, [H]).
generarAcumulada([], _VA, T)-> T;
generarAcumulada([H|T], VA, Tabla)-> generarAcumulada(T, H+VA, Tabla++[H+VA]).

%Genera una tabla con las probabilidades de selección de cada individuo.
generarTabla([Individuo|Resto])-> generarTabla(Resto, [Individuo|Resto], [((length(Individuo)*length(Individuo)) - fitness(Individuo))/sumaFitness([Individuo|Resto])]).
generarTabla([], _P, T)-> T;
generarTabla([Individuo|Resto], P, T)-> generarTabla(Resto, P, T++[((length(Individuo)*length(Individuo)) - fitness(Individuo))/sumaFitness(P)]).

%Selecciona un individuo a partir de una tabla acumulada y un valor al azar.
%Recibe: Tabla =  Tabla acumulada.
%		 P = Población.
%		 Valor = Valor al azar.
%Retorna: El individuo seleccionado a partir de la probabilidad.
seleccionarDeTabla([], _P, _Valor)->[];
seleccionarDeTabla([H|_T], [Individuo|_Resto], Valor) when H >= Valor-> Individuo;
seleccionarDeTabla([H|T], [_Individuo|Resto], Valor) when H < Valor-> seleccionarDeTabla(T, Resto, Valor).

%Función que realiza la selección de los 2 genes a cruzar.
%Recibe: P = Población
%		 TP: Tamaño de la población.
%Retorna: Una lista con los dos individuos a cruzar.
seleccion([Individuo|Resto], TP) when TP >= 2 -> seleccion([Individuo|Resto], TP, [seleccionarDeTabla(generarAcumulada(generarTabla([Individuo|Resto])), [Individuo|Resto], random:uniform())]);
seleccion([Individuo|_Resto], _TP) -> Individuo.
%Sele: Primer individuo selecto(para eliminar de la población).
seleccion([Individuo|Resto], _TP, Sele)-> Sele++[seleccionarDeTabla(generarAcumulada(generarTabla(eliminarDePoblacion([Individuo|Resto], Sele))), eliminarDePoblacion([Individuo|Resto], Sele), random:uniform())].

%Función que realiza el corte de los genes para el cruce.
%Recibe: G1 = Primer gen a realizarle el corte.
%		 G2 = El segundo gen a realizarle el corte.
%		 N = El tamaño de los genes.
%Retorna: Una lista con 4 elementos, los 2 cortes de cada gen.
corte(G1, G2) -> {split(length(G1) div 2, G1), split(length(G2) div 2, G2)}.

%Función que muta un gen.
%Recibe: G = Gen a mutar.
%		 N = Largo del gen.
mutar(G, N) -> mutarEnPosicion(G, N, random:uniform(N), 0, []).%Se selecciona la posición de forma random.

%Función que muta el alelo de gen en una posicón dada.
%Recibe: G = Gen a mutar.
%		 N = Largo del gen.
%		 Pos = Posición a modificar.
%		 C = Contador de la posición.
%		 Res = Gen mutado.
mutarEnPosicion([], _N, _Pos, _C, Res)->Res;
mutarEnPosicion([_H|T], N, C, C, Res)-> mutarEnPosicion(T, N, C, C+1, append(Res, [random:uniform(N)]));
mutarEnPosicion([H|T], N, Pos, C, Res)-> mutarEnPosicion(T, N, Pos, C+1, append(Res, [H])).

%Función que realiza el cruce de individuos.
%Recibe: G1 = El primer gen a cruzar.
%		 G2 = El segundo gen a cruzar.
%		 P = Probabilidad de mutación.
%Retorna: Lista con los 2 nuevos individuos.
cruce(P, Prob) -> cruce(getAt(P, 1), getAt(P, 2), Prob).
cruce(G1, G2, P) -> cruce_aux(G1, G2, P, random:uniform()).

cruce_aux(G1, G2, P, Rand) when P >= Rand -> cruce(corte(mutar(G1, length(G1)), G2));
cruce_aux(G1, G2, _P, _Rand) -> cruce(corte(G1, G2)). 

cruce(M) -> append([element(1, element(1, M)) ++ element(2, element(2, M))], [element(1, element(2, M)) ++ element(2, element(1, M))]).


%Función que agrega el cruce a la nueva población.
%Recibe: P = La nueva población.
%		 L = Lista con los individuos resultantes del cruce.
%Retorna: La población con el cruce agregado.
agregarCruce(_P, [])->[];
agregarCruce(P, [[H|T]|REST])-> append(append(P, [[H|T]]), REST). 

%Función que crea una nueva generación de individuos.
%Recibe: Pv = La población vieja.
%		 N = Cantidad de individuos a crear.
%		 Mut = Probabilidad de mutación.
%Retorna: Una lista de individuos, lo que es la nueva población.
crearNuevaPoblacion(PV, N, Mut, si) -> crearNuevaPoblacion(PV, N, Mut, 	cruce(seleccion(PV, length(PV)), Mut), [mejorDePoblacion(PV)]);
crearNuevaPoblacion(PV, N, Mut, _Elit) -> crearNuevaPoblacion(PV, N, Mut, cruce(seleccion(PV, length(PV)), Mut), []).
%NP: La nueva población.
crearNuevaPoblacion(_PV, N, _Mut, _Cru, NP) when length(NP) >= N -> NP;
crearNuevaPoblacion(PV, N, Mut, Cru, NP) -> crearNuevaPoblacion(PV, N, Mut, cruce(seleccion(PV, length(PV)), Mut), agregarCruce(NP, Cru)).

%Función que indica si hay un individuo solución dentro de una pobalación.
%Recibe: Población.
%Retorna: El individuo solución.
solucion([Individuo|Resto]) -> solucion(Resto, fitness(Individuo), Individuo).
solucion([], 0, S) -> S; 
solucion([], _F, _S) -> [];
solucion([Individuo|Resto], F, _S) when F > 0 -> solucion(Resto, fitness(Individuo), Individuo);
solucion([_Individuo|_Resto], 0, S) -> S.

nreinas(N, Pob, Mut, {limit, Gen, Elit}) -> nreinas_gen(N, Pob, Mut, Gen, Elit);
nreinas(N, Pob, Mut, {unlimit, Elit}) -> nreinas_inf(N, Pob, Mut, Elit);
nreinas(N, Pob, Mut, {hilos, Cant, Tup}) -> nreinas_hilos(Cant, [N, Pob, Mut, Tup]).

%Función que a partir de una población al azar genera una solución.
%Recibe: N = Tamaño del tablero.
%		 Pob = Tamaño de la población.
%		 Mut = Probabilidad de mutación.
nreinas_gen(N, Pob, Mut, MaxGen, Elit) -> nreinas_gen(N, Pob, Mut, generarPoblacion(N, Pob), MaxGen, Elit).
nreinas_gen(N, Pob, Mut, P, MaxGen, Elit) -> nreinas_gen(N, Pob, Mut, P, 1, MaxGen, Elit, solucion(P)).
nreinas_gen(N, Pob, Mut, P, NGen, MaxGen, Elit, []) -> nreinas_gen(N, Pob, Mut, crearNuevaPoblacion(P, Pob, Mut, Elit), NGen+1, MaxGen, Elit, solucion(P));
nreinas_gen(_N, _Pob, _Mut, _P, MaxGen, MaxGen, _Elit, S) -> S, io:format("~p~n~n", ["Sin solucion"]);
nreinas_gen(_N, _Pob, _Mut, _P, _NGen, _MaxGen, _Elit, S) -> printIndividuo(S), io:format("~p~n~n", [printIndividuo(S)]).

nreinas_inf(N, Pob, Mut, Elit) -> nreinas_inf(N, Pob, Mut, generarPoblacion(N, Pob), Elit).
nreinas_inf(N, Pob, Mut, P, Elit) -> nreinas_inf(N, Pob, Mut, P, Elit, solucion(P)).
nreinas_inf(N, Pob, Mut, P, Elit, []) -> nreinas_inf(N, Pob, Mut, crearNuevaPoblacion(P, Pob, Mut, Elit), Elit, solucion(P));
nreinas_inf(_N, _Pob, _Mut, _P, _Elit, S) -> printIndividuo(S), io:format("~p~n~n", [printIndividuo(S)]).

nreinas_hilos(1, Params) -> spawn(geneticos, nreinas, Params);
nreinas_hilos(N, Params) -> spawn(geneticos, nreinas, Params), nreinas_hilos(N-1, Params).