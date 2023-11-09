#include <iostream>
#include "LinkedList.h"
using namespace std;

struct NodoL {
int elem; // valor del nodo
NodoL* siguiente; // puntero al siguiente nodo
};
struct LinkedListSt {
// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
// desde primero por siguiente hasta alcanzar a NULL
int cantidad; // cantidad de elementos
NodoL* primero; // puntero al primer nodo
NodoL* ultimo;
};
struct ListIteratorSt {
NodoL* current;
};

LinkedList emptyList(){
    LinkedListSt* list = new LinkedListSt;
    list->cantidad = 0;
    list->primero = NULL;
    list->ultimo = NULL;
};

bool isEmptyList(LinkedList xs){
    return xs->cantidad == 0;
}

int head(LinkedList xs){
    xs->primero;
}

void Cons(int n, LinkedList xs){
    NodoL* nodoN = new NodoL;
    nodoN->elem = n;
    nodoN->siguiente = xs->primero;
    if(xs->ultimo == NULL){
        xs->ultimo == nodoN;
    }
    xs->primero = nodoN;
    xs->cantidad++;
}

void Tail(LinkedList xs){
    NodoL* temp = xs->primero;
    xs->primero = xs->primero->siguiente;
    if(not(xs->primero == NULL)){
            xs->cantidad--;
    }
}

int length(LinkedList xs){
    return xs->cantidad;
}

void Snoc(LinkedList xs, int n){
    NodoL* nodoN = new NodoL;
    nodoN->elem = n;
    nodoN->siguiente = NULL;
    if(xs->primero == NULL){
        xs->primero = nodoN;
    }
    else{xs->ultimo->siguiente = nodoN;}
    xs->ultimo = nodoN;
    xs->cantidad++;
}

void DestroyL(LinkedList xs){
    delete xs;
}

void append(LinkedList xs, LinkedList ys){
    xs->ultimo->siguiente = ys->primero;
    xs->cantidad++;
}

//------------------------ITERADOR----------------------------//
ListIterator getIterator(LinkedList xs){
    ListIteratorSt* iterator = new ListIteratorSt;
    iterator->current = xs->primero;
}

int current(ListIterator ixs){
    ixs->current->elem;
}

void setCurrent(int x, ListIterator ixs){
    NodoL* nodoN = new NodoL;
    ixs->current = nodoN;
}

void Next(ListIterator ixs){
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs){
    return ixs->current == NULL;
}

void DisposeIterator(ListIterator ixs){
    delete ixs;
}


