-module(geneticos).
-import(lists,[append/2, split/2, seq/2, nth/2]).
-import(string,[concat/2]).
-export([nreinas/4]). 

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

shuffle(L) -> shuffle(L, []).
shuffle([], Acc) -> Acc;
shuffle(L, Acc) -> {Leading, [H|T]} = lists:split(random:uniform(length(L))-1, L), shuffle(Leading ++ T, [H|Acc]).

swap(List, S1, S2) -> {List2,[F|List3]} = lists:split(S1-1,List),
   LT = List2++[lists:nth(S2,List)|List3],
   {List4,[_|List5]} = lists:split(S2-1,LT),
   List4++[F|List5].

% Crea la población inicial.
% Recibe: Tab = Tamaño del tablero.
%         Pob = Tamaño de la población.
% Retorna: Lista de individuos.
generarPoblacion(Tab, Pob) -> [crearIndividuo(Tab) || _X <- seq(1, Pob)].

% Crea un individuo.
% Recibe: N = Cantidad de genes del individuo.
% Retorna: Nuevo individuo.
crearIndividuo(N) -> shuffle(seq(1,N)).

fitness([]) -> 0;
fitness([Reina|RestoTablero]) -> cant_choca(Reina, RestoTablero) + fitness(RestoTablero).

fitnessMax(Individuo) -> math:pow(length(Individuo), 2) - fitness(Individuo).

% Indica si una reina choca con otra en la misma columna.
% Recibe: R = Reina a revisar con los demás números del individuo.
chocaColumna(Reina, Reina, _Profundidad) -> 1;
chocaColumna(_Reina, _Tablero, _Profundidad) -> 0.

% Indica la candtidad de veces que una reina choca con otra en la diagonal izquierda.
% Recibe: Reina = Reina a revisar.
%		      N = Fila donde está la reina.
%		      Profundidad = 
chocaDiagonalIzquierda(Reina, Fila, Profundidad) when Reina == Fila - Profundidad -> 1;
chocaDiagonalIzquierda(_Reina, _Tablero, _Profundidad) -> 0.

% Indica la candtidad de veces que una reina choca con otra en la diagonal derecha.
% Recibe: Reina = Reina a revisar.
%		      N = Fila donde está la reina.
%		      Profundidad = 
chocaDiagonalDerecha(Reina, Fila, Profundidad) when Reina == Fila + Profundidad -> 1;
chocaDiagonalDerecha(_Reina, _Tablero, _Profundidad) -> 0.

% Indica la cantidad de veces que chocan las reinas en un individuo.
% Recibe: Reina = Reina a revisar.
%	        Tablero = El tablero a revisar.
% Retorna: Cnatidad de veces que choca una reina en el tablero.
cant_choca(Reina, Tablero) -> cant_choca(Reina, Tablero, 1).
cant_choca(_Reina, [], _Profundidad) -> 0;
cant_choca(Reina, [Fila|RestoTablero], Profundidad) -> chocaColumna(Reina, Fila, Profundidad)  + chocaDiagonalIzquierda(Reina, Fila, Profundidad) + chocaDiagonalDerecha(Reina, Fila, Profundidad) + cant_choca(Reina, RestoTablero, Profundidad+1).

% Función que indica el mejor individuo de una población.
% Recibe: Poblacion = La Poblacion.
mejorDePoblacion([Individuo|RestoPoblacion]) -> mejorDePoblacion( [[X, fitness(X)] || X <- RestoPoblacion], [Individuo, fitness(Individuo)]).
mejorDePoblacion([], [Individuo|_Fitness]) -> Individuo;
mejorDePoblacion([[Individuo|Fitness]|Resto], [_MinIndividuo|MinFitness]) when Fitness < MinFitness -> mejorDePoblacion(Resto, [Individuo|Fitness]);
mejorDePoblacion([_Individuo|Resto], MinIndividuo) -> mejorDePoblacion(Resto, MinIndividuo).

% Suma el fitness de todos los individuos de una población.
% Para maximizar el fitness es la cantidad de veces que choca menos el tamaño del tablero N*N, no solamente N.
sumaFitness([]) -> 0;
sumaFitness([Individuo|Resto]) -> fitnessMax(Individuo) + sumaFitness(Resto).

% Genera una tabla acumulada de las probabilidades de cada individuo.
generarAcumulada([H|T]) -> generarAcumulada(T, H, [H]).
generarAcumulada([], _VA, T) -> T;
generarAcumulada([H|T], VA, Tabla) -> generarAcumulada(T, H+VA, Tabla++[H+VA]).

% Genera una tabla con las probabilidades de selección de cada individuo.
generarTabla([Individuo|Resto]) -> generarTabla(Resto, [Individuo|Resto], [fitnessMax(Individuo)/sumaFitness([Individuo|Resto])]).
generarTabla([], _P, T) -> T;
generarTabla([Individuo|Resto], P, T) -> generarTabla(Resto, P, T++[fitnessMax(Individuo)/sumaFitness(P)]).

% Selecciona un individuo a partir de una tabla acumulada y un valor al azar.
% Recibe: Tabla =  Tabla acumulada.
%		      P = Población.
%		      Valor = Valor al azar.
% Retorna: El individuo seleccionado a partir de la probabilidad.
seleccionarDeTabla([], _P, _Valor) -> [];
seleccionarDeTabla([H|_T], [Individuo|_Resto], Valor) when H >= Valor -> Individuo;
seleccionarDeTabla([_H|T], [_Individuo|Resto], Valor) -> seleccionarDeTabla(T, Resto, Valor).

% Función que realiza la selección de los 2 individuos a cruzar.
% Recibe: P = Población
% Retorna: Una lista con los dos individuos a cruzar.
seleccion(Poblacion) ->  A = generarAcumulada(generarTabla(Poblacion)), [seleccionarDeTabla(A, Poblacion, random:uniform()), seleccionarDeTabla(A, Poblacion, random:uniform())].

% Función que muta un individuo.
% Recibe: Individuo = Individuo a mutar.
mutar(Individuo) -> N = length(Individuo), swap(Individuo, random:uniform(N), random:uniform(N)).

% Función que realiza el corte de los individuos para el cruce.
% Recibe: I1 = Primer individuo a realizarle el corte.
%		      I2 = El segundo individuo a realizarle el corte.
% Retorna: Una tupla con los cortes de cada individuo.
corte(I1, I2) -> {split(length(I1) div 2, I1), split(length(I2) div 2, I2)}.

juntar(T) -> append([element(1, element(1, T)) ++ element(2, element(2, T))], [element(1, element(2, T)) ++ element(2, element(1, T))]).

% Función que realiza el cruce de individuos.
% Recibe: I1 = El primer individuo a cruzar.
%		      I2 = El segundo individuo a cruzar.
%		      Mut = Probabilidad de mutación.
% Retorna: Lista con los 2 nuevos individuos.
cruce(Individuos, Mut) -> cruce(nth(1, Individuos), nth(2, Individuos), Mut).
cruce(I1, I2, Mut) -> cruce(I1, I2, Mut, random:uniform()).
cruce(I1, I2, Mut, Rand) when Rand < Mut -> juntar( corte(mutar(I1), I2) );
cruce(I1, I2, _Mut, _Rand) -> juntar( corte(I1, I2) ). 

% Función que agrega el cruce a la nueva población.
% Recibe: P = La nueva población.
%		      L = Lista con los individuos resultantes del cruce.
% Retorna: La población con el cruce agregado.
agregarCruce(_P, [])->[];
agregarCruce(P, [[H|T]|REST])-> append(append(P, [[H|T]]), REST). 

% Función que crea una nueva generación de individuos.
% Recibe: PV = La población vieja.
%		      N = Cantidad de individuos a crear.
%		      Mut = Probabilidad de mutación.
% Retorna: Una lista de individuos, lo que es la nueva población.
crearNuevaPoblacion(PV, N, Mut, si) -> crearNuevaPoblacion(PV, N, Mut, cruce(seleccion(PV), Mut), [mejorDePoblacion(PV)]);
crearNuevaPoblacion(PV, N, Mut, _Elit) -> crearNuevaPoblacion(PV, N, Mut, cruce(seleccion(PV), Mut), []).
crearNuevaPoblacion(_PV, N, _Mut, _Cru, NP) when length(NP) >= N -> NP;
crearNuevaPoblacion(PV, N, Mut, Cru, NP) -> crearNuevaPoblacion(PV, N, Mut, cruce(seleccion(PV), Mut), agregarCruce(NP, Cru)).

% Función que indica si hay un individuo solución dentro de una población.
% Recibe: Población.
% Retorna: El individuo solución.
solucion(Poblacion) -> solucion_aux([[X, fitness(X)] || X <- Poblacion]).
solucion_aux([]) -> [];
solucion_aux([[Individuo, 0]|_Resto]) -> Individuo;
solucion_aux([[_Individuo, _Fitness]|Resto]) -> solucion_aux(Resto).

nreinas(N, Pob, Mut, {limit, Gen, Elit}) -> random:seed(erlang:now()), nreinas_gen(N, Pob, Mut, Gen, Elit);
nreinas(N, Pob, Mut, {unlimit, Elit}) -> random:seed(erlang:now()), nreinas_inf(N, Pob, Mut, Elit);
nreinas(N, Pob, Mut, {hilos, Cant, Tup}) -> random:seed(erlang:now()), nreinas_hilos(Cant, [N, Pob, Mut, Tup]).

nreinas_gen(N, Pob, Mut, MaxGen, Elit) -> nreinas_gen(N, Pob, Mut, generarPoblacion(N, Pob), MaxGen, Elit).
nreinas_gen(N, Pob, Mut, P, MaxGen, Elit) -> nreinas_gen(N, Pob, Mut, P, 1, MaxGen, Elit, solucion(P)).
nreinas_gen(_N, _Pob, _Mut, _P, MaxGen, MaxGen, _Elit, S) -> S, io:format("~p~n~n", [S]);
nreinas_gen(N, Pob, Mut, P, NGen, MaxGen, Elit, []) -> nreinas_gen(N, Pob, Mut, crearNuevaPoblacion(P, Pob, Mut, Elit), NGen+1, MaxGen, Elit, solucion(P));
nreinas_gen(_N, _Pob, _Mut, _P, _NGen, _MaxGen, _Elit, S) -> printIndividuo(S), io:format("~p~n~n", [printIndividuo(S)]).

nreinas_inf(N, Pob, Mut, Elit) -> nreinas_inf(N, Pob, Mut, generarPoblacion(N, Pob), Elit).
nreinas_inf(N, Pob, Mut, P, Elit) -> nreinas_inf(N, Pob, Mut, P, Elit, solucion(P)).
nreinas_inf(N, Pob, Mut, P, Elit, []) -> nreinas_inf(N, Pob, Mut, crearNuevaPoblacion(P, Pob, Mut, Elit), Elit, solucion(P));
nreinas_inf(_N, _Pob, _Mut, _P, _Elit, S) -> printIndividuo(S), io:format("~p~n~n", [printIndividuo(S)]).

nreinas_hilos(1, Params) -> spawn(geneticos, nreinas, Params);
nreinas_hilos(N, Params) -> spawn(geneticos, nreinas, Params), nreinas_hilos(N-1, Params).
