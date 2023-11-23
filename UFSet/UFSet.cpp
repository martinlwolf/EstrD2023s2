#include "UFSet.h"

/*INV. REP.:
-Todo UFSet tiene un padre, por lo que el padre de cada uno no puede ser NULL en ningun caso.
-El elemento de un UFSet tampoco puede ser NULL, por lo que el UFSet nunca puede ser vacio.
-El rango solo puede incrementar, en ningun caso puede ser negativo, y siempre es 0 cuando se crea el UFSet.*/

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
}
//Costo O(1), la creacion del UFSet en la memoria heap y el seteo de variables es de costo constante

ELEM_TYPE elemUFS(UFSet ufset) {
   return ufset->element;
}
//Costo O(1), el acceso a la variable es constante

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

//Costo O(N), teniendose que recorrer cada uno de los "padres" de los nodos hasta llegar a la raiz. Esto se hace 2 veces pero se simplifica 
//como un costo lineal.



//1ER UNION ANTES DE OPTIMIZAR POR RANGO
/*void unionUFS(UFSet ufset1, UFSet ufset2) {
   findUFS(ufset1)->padre = findUFS(ufset2);
}*/
void unionUFS(UFSet ufset1, UFSet ufset2) {
   UFSet raizUF1 = findUFS(ufset1);
   UFSet raizUF2 = findUFS(ufset2);
   if(raizUF1 == raizUF2){
      ;
   }
   else if (raizUF1->rango > raizUF2->rango){
      raizUF2->padre = raizUF1;
   }
   else if(raizUF1->rango < raizUF2->rango){
      raizUF1->padre = raizUF2;
   }
   else{
      raizUF2->padre = raizUF1;
      raizUF1->rango++;
   }
}

//Costo O(N), por tener que realizar 2 veces la operiacion findUFS. El resto de las operaciones de modificacion de variables son constantes.



