#include "UFSet.h"

/*
 * UFSet.cpp contiene la implementación de la interfaz para UFSet declarada en UFSet.h. 
 * Deben implementarse las operaciones de acuerdo a la representación elegida para el tipo UFSet.
 */

/* El tipo UFNode* representa:
 *  1. un elemento de un UFSet (o sea, un nodo del árbol que contiene a todos los elementos del conjunto)
 *  2. al conjunto en su totalidad, si el nodo es la raíz del arbol
 *
 *  El nodo tiene un puntero a su elemento asociado en el campo element. 
 *  Deberán agregarse los campos necesarios para completar la representación.
 */
struct UFNode {
   ELEM_TYPE element;
   UFSet padre;
   // COMPLETAR
};

/* 
 * Inicializa el UFSet ufset, cuyo valor asociado será value 
 */
UFSet createUFS(ELEM_TYPE value) {
   UFNode* newUFSet = new UFNode;
   newUFSet->element = value;
   newUFSet->padre = NULL;
   // COMPLETAR
}

ELEM_TYPE elemUFS(UFSet ufset) {
   return ufset->element;
   // COMPLETAR
}

/*
 * Encuentra el elemento distinguido para el UFSet dado. 
 * Esta operación puede ser optimizada con la técnica de compresión de camino.
 */
//Cuando no tenga un padre, significa que el nodo es el elemento distinguido
UFSet findUFS(UFSet elem) {
   UFNode* nodeActual = elem;
   while(nodeActual->padre != NULL){
      nodeActual = nodeActual->padre;
   }
   return nodeActual;
   // COMPLETAR
}
//Costo O(N), teniendose que recorrer toda una "rama" en caso de que el set dado sea una "leave"

/*
 * Calcula la unión entre los conjuntos ufset1 y ufset2. 
 * Esta operación puede ser optimizada con la técnica de unión por rango.
 */
void unionUFS(UFSet ufset1, UFSet ufset2) {
   findUFS(ufset1)->padre = findUFS(ufset2);
   // COMPLETAR
}

//Costo O(N), por realizar 2 veces findUFS de costo O(N)
