-module(geneticos).
-import(lists,[append/2]).
-import(string,[concat/2]).
-export([generarPoblacion/2, chocaColumna/2, cant_choca/1, choca/1, chocaDiagonal/2, getAt/2, mejorDePoblacion/1, seleccion/2, eliminarDePoblacion/2]). 
-export([fitness2/1, fitness/1, shuffle/1, corte/3, cruce/3, reemplazarEnPoblacion/3, agregarCruce/2, crearNuevaPoblacion/3, mutar/2, crearFila/2, printIndividuo/1, imprimirPoblacion/1]).

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
% Recibe: N = Tamaño del individuo.
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

%Indica si una reina choca con otra en la misma columna.
%Recibe: L = Lista o individuo a revisar.
%		 R = Reina a revisar con los demás números del individuo.
chocaColumna([], _R)-> false;
chocaColumna([H|T], R)->
 	if(H =:= R)-> true;
 		true-> chocaColumna(T, R)
 	end.

%Indica si una reina choca con otra en diagonal.
%Recibe: L = Lista o individuo a revisar.
%		 N = Número a revisar con los demás números del individuo.
chocaDiagonal([], _N)->false;
chocaDiagonal(L, B)-> chocaDiagonal(L, B, 1).

%Indica si una reina choca con otra en diagonal.
%Recibe: L = Lista o individuo a revisar.
%		 R = Reina a revisar.
%		 N = nésima reina del individuo.
chocaDiagonal([], _R, _N)-> false;
chocaDiagonal([H|T], R, N)->
if((H =:= (R+N)) or (H =:= (R-N)))-> true;
	true->chocaDiagonal(T, R, N+1)
end.

%Indica si una reina choca con otra.
%Recibe: L = Lista o individuo a revisar.
choca([])->false;
choca([H|T])-> choca(T, chocaColumna(T, H) or chocaDiagonal(T, H)).

%Indica si una reina choca con otra.
%Recibe: L = Lista o individuo a revisar.
%		 Collide = valor que indica si choca.
choca([], _Collide)->false;
choca([H|T], Collide)->
	if(Collide)->true;
		true->choca(T, chocaColumna(T, H) or chocaDiagonal(T, H))
	end.

%Indica la cantidad de veces que una reina choca con otra en un individuo.
%Recibe: L = Lista o individuo a revisar.
cant_choca([]) -> 0;
cant_choca([H|T])-> cant_choca(T, chocaColumna(T, H) or chocaDiagonal(T, H), 0).

%Indica la cantidad de veces que chocan las reinas en un individuo.
%Recibe: L = Lista o individuo a revisar.
%		 Collide = valor que indica si choca.
%		 C = Contador de veces que choca la reina.
%Retorna: Cnatidad de veces que chocan las reinas en un individuo.
cant_choca([], _Collide, C)->C;
cant_choca([H|T], Collide, C)->
	if(Collide)->cant_choca(T, chocaColumna(T, H) or chocaDiagonal(T, H), C+1);
		true->cant_choca(T, chocaColumna(T, H) or chocaDiagonal(T, H), C)
	end.

%Función de fitness.
fitness([H|T])->cant_choca([H|T]).

fitness2([]) -> 0;
fitness2([Reina|RestoTablero]) -> cant_choca2(Reina, RestoTablero) + fitness2(RestoTablero).

chocaColumna2(Reina, Reina, _Profundidad) -> 1;
chocaColumna2(_Reina, _Tablero, _Profundidad) -> 0.

chocaDiagonalIzquierda(Reina, Fila, Profundidad) when Reina == Fila - Profundidad -> 1;
chocaDiagonalIzquierda(_Reina, _Tablero, _Profundidad) -> 0.

chocaDiagonalDerecha(Reina, Fila, Profundidad) when Reina == Fila + Profundidad -> 1;
chocaDiagonalDerecha(_Reina, _Tablero, _Profundidad) -> 0.

cant_choca2(Reina, Tablero) -> cant_choca2(Reina, Tablero, 1).
cant_choca2(_Reina, [], _Profundidad) -> 0;
cant_choca2(Reina, [Fila|RestoTablero], Profundidad) -> chocaColumna2(Reina, Fila, Profundidad)  + chocaDiagonalIzquierda(Reina, Fila, Profundidad) + chocaDiagonalDerecha(Reina, Fila, Profundidad) + cant_choca2(Reina, RestoTablero, Profundidad+1).

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


%Función que realiza la selección de los 2 genes a cruzar.
%Recibe: P = Población
%		 TP: Tamaño de la población.
%Retorna: Una lista con los dos individuos a cruzar.
seleccion([[H|T]|REST], TP)->
if (TP >= 2)->append([getAt([[H|T]|REST], random:uniform(TP))], [getAt([[H|T]|REST], random:uniform(TP))]);
	true->[H|T]
end.

%Función que realiza el corte de los genes para el cruce.
%Recibe: G1 = Primer gen a realizarle el corte.
%		 G2 = El segundo gen a realizarle el corte.
%		 N = El tamaño de los genes.
%Retorna: Una lista con 4 elementos, los 2 cortes de cada gen.
corte(G1, G2, N)-> corte(G1, G2, random:uniform(N), 0, [], []).
corte(G1, G2, N, N, P1, P2)-> append(append([G1], [P1]), append([G2], [P2]));
corte([H|T], [H2|T2], N, C, P1, P2)-> corte(T, T2, N, C+1, append([H], P1), append([H2], P2)).


%Función que muta un gen.
%Recibe: G = Gen a mutar.
%		 N = Largo del gen.
mutar(G, N)->mutarEnPosicion(G, N, random:uniform(N), 0, []).%Se selecciona la posición de forma random.

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
cruce(G1, G2, P)->cruce_aux(G1, G2, P, random:uniform()).
cruce_aux(G1, G2, P, Rand)->
if(P >= Rand)-> cruce(corte(mutar(G1, length(G1)), G2, length(G1)));
	true->cruce(corte(G1, G2, length(G1)))
end.
cruce(M)-> append([getAt(M,1)++getAt(M,4)], [getAt(M,2)++getAt(M,3)]).

%Función que determina el peor de 2 genes.
%Recibe: G1 = Primer gen a evaluar.
%		 G2 = Segundo gen a evaluar.
%Retorna el peor gen de los 2. 
peorGen(G1, G2)-> peorGen(G1, fitness(G1), G2, fitness(G2)).
peorGen(G1, F1, _G2, F2) when F1 >= F2 -> G1;
peorGen(_G1, F1, G2, F2) when F1 < F2 -> G2.


%Función que reemplaza el peor gen de una plobación por el mejor gen.
%Recibe: P = Población.
%		 M = Matriz o lista de genes, que contiene el resultado de la selcción.
%Retorna: La población con el individuo reemplazado.
reemplazarEnPoblacion(P, M)-> reemplazarEnPoblacion(P, peorGen(getAt(M, 1), getAt(M, 2)), mejorDePoblacion(M)).
%E: Individuo a reemplazar.
%R: Individuo a agregar por el eliminado.
reemplazarEnPoblacion(P, E, R)-> reemplazarEnPoblacion_aux(eliminarDePoblacion(P, E), R).
reemplazarEnPoblacion_aux(P, R)-> append(P, [R]).


%Función que agrega el cruce a la nueva población.
%Recibe: P = La nueva población.
%		 L = Lista con los individuos resultantes del cruce.
%Retorna: La población con el cruce agregado.
agregarCruce(_P, [])->[];
agregarCruce(P, [[H|T]|REST])-> append(append(P, [[H|T]]), REST). 


%Función que crea una nueva generación de individuos.
%Recibe: Pv = La población vieja.
%		 N = Cantidad de individuos a crear.
%		 P = Probabilidad de mutación.
%Retorna: Una lista de individuos, lo que es la nueva población.
crearNuevaPoblacion(PV, N, P)-> crearNuevaPoblacion(PV, N, P, 0, seleccion(PV, N)).
%C: Contador de individuos.
%Sele: Resultado de la selección.
crearNuevaPoblacion(PV, N, P, C, [[H|T]|REST])-> crearNuevaPoblacion(reemplazarEnPoblacion(PV, [[H|T]|REST]), N, P, C, 
													seleccion(reemplazarEnPoblacion(PV, [[H|T]|REST]), N), cruce(getAt([[H|T]|REST], 1), getAt([[H|T]|REST], 2), P),
													[]).
%Cru: Resultado del cruce.
%NP: La nueva población.
crearNuevaPoblacion(_PV, N, _P, C, _Sele, _Cru, NP) when C >= N -> NP;
crearNuevaPoblacion(PV, N, P, C, [[H|T]|REST], Cru, NP)-> crearNuevaPoblacion(reemplazarEnPoblacion(PV, [[H|T]|REST]), N, P, C+2, 
													seleccion(reemplazarEnPoblacion(PV, [[H|T]|REST]), N), cruce(getAt([[H|T]|REST], 1), getAt([[H|T]|REST], 2), P),
													agregarCruce(NP, Cru)).



 

