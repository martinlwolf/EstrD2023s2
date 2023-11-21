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
   int rango;
};

/* 
 * Inicializa el UFSet ufset, cuyo valor asociado será value 
 */
UFSet createUFS(ELEM_TYPE value) {
   UFNode* newUFSet = new UFNode;
   newUFSet->element = value;
   newUFSet->padre = newUFSet;
   newUFSet->rango = 0;
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
//1ER FIND ANTES DE OPTIMIZAR POR COMPRESION
/*UFSet findUFS(UFSet elem) {
   UFNode* nodeActual = elem;
   while(nodeActual->padre != nodeActual){
      nodeActual = nodeActual->padre;
   }
   return nodeActual;
   
}*/

UFSet findUFS(UFSet elem) {
   UFNode* distinguido = elem;
   UFNode* nodo = elem;
   UFNode* temp = elem;
   //Se encuentra la raiz
   while(distinguido->padre != distinguido){
      distinguido = distinguido->padre;
   }
   //Se comprime cambiando los padres a la raiz
   while(nodo != distinguido){
      temp = nodo;
      nodo = nodo->padre;
      temp->padre = distinguido;
   }
   return distinguido;
}

//Costo O(N), teniendose que recorrer toda una "rama" en caso de que el set dado sea una "leave"

/*
 * Calcula la unión entre los conjuntos ufset1 y ufset2. 
 * Esta operación puede ser optimizada con la técnica de unión por rango.
 */
//1ER UNION ANTES DE OPTIMIZAR POR RANGO
/*void unionUFS(UFSet ufset1, UFSet ufset2) {
   findUFS(ufset1)->padre = findUFS(ufset2);
   // COMPLETAR
}*/
void unionUFS(UFSet ufset1, UFSet ufset2) {
   if(findUFS(ufset1) == findUFS(ufset2)){
      ;
   }
   else if (findUFS(ufset1)->rango > findUFS(ufset2)->rango){
      findUFS(ufset2)->padre = findUFS(ufset1);
   }
   else if(findUFS(ufset1)->rango < findUFS(ufset2)->rango){
      findUFS(ufset1)->padre = findUFS(ufset2);
   }
   else{
      findUFS(ufset2)->padre = findUFS(ufset1);
      findUFS(ufset1)->rango++;
   }
   //poner los ufset en variables
}


//Costo O(N), por realizar 2 veces findUFS de costo O(N)
