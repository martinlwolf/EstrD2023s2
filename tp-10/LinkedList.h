#include <iostream>
using namespace std;

struct  LinkedListSt; 
typedef LinkedListSt* LinkedList;           // INV.REP.: el puntero NO es NULL

struct  ListIteratorSt;
typedef ListIteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL


LinkedList emptyList();
void Cons(int n, LinkedList xs);  
void Snoc(LinkedList xs, int n);  
bool isEmptyList(LinkedList xs);
int  head(LinkedList xs); // PRECOND: lista no vacía
void Tail(LinkedList xs); // PRECOND: lista no vacía
int  length(LinkedList xs);
void DestroyL(LinkedList xs);
//Libera la memoria ocupada por la lista.

ListIterator getIterator(LinkedList xs);
//Apunta el recorrido al primer elemento.
int current(ListIterator ixs);
//Devuelve el elemento actual en el recorrido.
void setCurrent(int x, ListIterator ixs);
//Reemplaza el elemento actual por otro elemento.
void Next(ListIterator ixs);
//Pasa al siguiente elemento.
bool atEnd(ListIterator ixs);
//Indica si el recorrido ha terminado.
void DisposeIterator(ListIterator ixs);
//Libera la memoria ocupada por el iterador.
