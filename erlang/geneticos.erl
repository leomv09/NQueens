-module(geneticos).
-import(lists,[append/2]).
-import(string,[concat/2]).
-export([generarPoblacion/2, chocaColumna/3, cant_choca/2,  chocaDiagonalDerecha/3, chocaDiagonalIzquierda/3, getAt/2, mejorDePoblacion/1, seleccion/2, eliminarDePoblacion/2]). 
-export([fitness/1, shuffle/1, corte/3, cruce/3, agregarCruce/2, crearNuevaPoblacion/3, mutar/2, crearFila/2, printIndividuo/1, imprimirPoblacion/1]).
-export([solucion/1, nreinas/3, generarTabla/1, sumaFitness/1, generarAcumulada/1, seleccionarDeTabla/3]).
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




%Función de fitness.

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
sumaFitness([])->0;
sumaFitness([Individuo|Resto])-> (length(Individuo)-fitness(Individuo)) + sumaFitness(Resto).

%Genera una tabla acumulada de las probabilidades de cada individuo.
generarAcumulada([H|T])-> generarAcumulada(T, H, [H]).
generarAcumulada([], _VA, T)-> T;
generarAcumulada([H|T], VA, Tabla)-> generarAcumulada(T, H+VA, Tabla++[H+VA]).

%Genera una tabla con las probabilidades de selección de cada individuo.
generarTabla([Individuo|Resto])-> generarTabla(Resto, [Individuo|Resto], [(length(Individuo)-fitness(Individuo))/sumaFitness([Individuo|Resto])]).
generarTabla([], _P, T)-> T;
generarTabla([Individuo|Resto], P, T)-> generarTabla(Resto, P, T++[(length(Individuo)-fitness(Individuo))/sumaFitness(P)]).


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
seleccion([Individuo|Resto], TP)->
if (TP >= 2)->seleccion([Individuo|Resto], TP, [seleccionarDeTabla(generarAcumulada(generarTabla([Individuo|Resto])), [Individuo|Resto], random:uniform())]);
	true->Individuo
end.
%Sele: Primer individuo selecto(para eliminar de la población).
seleccion([Individuo|Resto], _TP, Sele)-> Sele++[seleccionarDeTabla(generarAcumulada(generarTabla(eliminarDePoblacion([Individuo|Resto], Sele))), eliminarDePoblacion([Individuo|Resto], Sele), random:uniform())].

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
crearNuevaPoblacion(PV, N, P)-> crearNuevaPoblacion(PV, N, P, 0, seleccion(PV, length(PV))).
%C: Contador de individuos.
%Sele: Resultado de la selección.
crearNuevaPoblacion(PV, N, P, C, [[H|T]|REST])-> crearNuevaPoblacion(PV, N, P, C, 
													seleccion(PV, length(PV)), cruce(getAt([[H|T]|REST], 1), getAt([[H|T]|REST], 2), P),
													[]).
%Cru: Resultado del cruce.
%NP: La nueva población.
crearNuevaPoblacion(_PV, N, _P, C, _Sele, _Cru, NP) when C >= N -> NP;
crearNuevaPoblacion(PV, N, P, C, [[H|T]|REST], Cru, NP)-> crearNuevaPoblacion(PV, N, P, C+2, 
													seleccion(PV, length(PV)), cruce(getAt([[H|T]|REST], 1), getAt([[H|T]|REST], 2), P),
													agregarCruce(NP, Cru)).


%Función que indica si hay un individuo solución dentro de una pobalación.
%Recibe: Población.
%Retorna: El individuo solución.
solucion([Individuo|Resto])->solucion(Resto, fitness(Individuo), Individuo).
solucion([], F, S)-> 
	if(F==0)->S;
		true->[]
	end;
solucion([Individuo|Resto], F, _S) when F > 0-> solucion(Resto, fitness(Individuo), Individuo);
solucion([_Individuo|_Resto], F, S) when F == 0-> S.

%Función que a partir de una población al azar genera una solución.
%Recibe: N = Tamaño del tablero.
%		 Pob = Tamaño de la población.
%		 Mut = Probabilidad de mutación.
nreinas(N, Pob, Mut)-> nreinas(N, Pob, Mut, generarPoblacion(N, Pob)).
nreinas(N, Pob, Mut, P)-> nreinas(N, Pob, Mut, P, solucion(P)).
nreinas(N, Pob, Mut, P, S)-> 
	if(S==[])-> nreinas(N, Pob, Mut, crearNuevaPoblacion(P, Pob, Mut), solucion(P));
		true-> S
	end.

