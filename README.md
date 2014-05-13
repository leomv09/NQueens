NQueens
=======

* Instituto Tecnológico de Costa Rica.
* Carrera Ingeniería en Computación.
* Lenguajes de Programación.
* III Proyecto Programado.
* I Semestre 2014.


###Integrantes

* Leonardo Madrigal Valverde.
* José Andrés García Sáenz.


##Descripción

Se debe de hacer una comparacion tanto en facilidad de escribir codigo como en
claridad de la solucion, pero tambien en el O(n) que implica encontrar 1 solucion, varias
soluciones o todas las soluciones entre dos aproximaciones clasicas.
La primera aproximacion es la implementacion por backtracking, para ello se utili-
zara el lenguaje de programacion Prolog, el cual debe de ser capaz de encontrar todas
las soluciones posibles para un problema de tama~no n cada vez que se oprima la opcion
or (;).
La segunda solucion consiste en la solucion por medio de algoritmos geneticos. En
esta aproximacion se utilizara el lenguaje de programacion Erlang. Esta tendra tres
modalidades, la primera consiste en dada una poblacion inicial aleatoria (pero valida
para ser solucion), generara a lo sumo k generaciones, mostrando el mejor de cada
generacion, si encuentra una solucion, entonces se detiene. La segunda forma es que no
se detenga hasta encontrar una solucion. La tercera forma es que se puedan hacer hilos
para llamar a cualquiera de las dos versiones anteriores. Notese que si se llaman a h hilos
con la segunda opcion, en principio puedo obtener hasta h soluciones diferentes.

