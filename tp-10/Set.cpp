#include <iostream>
#include "Set.h"
#include "LinkedList.cpp"

using namespace std;

struct NodoS {
int elem; // valor del nodo
NodoS* siguiente; // puntero al siguiente nodo
};
struct SetSt {
int cantidad; // cantidad de elementos diferentes
NodoS* primero; // puntero al primer nodo
};

Set emptyS(){
    SetSt* set = new SetSt;
    set->cantidad = 0;
    set->primero = NULL;
    return set;
}

bool isEmptyS(Set s){
    return s->cantidad == 0;
}

bool belongsS(int x, Set s){
    int counter = 1;
    bool belongs = false;
    NodoS* current = s->primero;
    while(counter<=s->cantidad && not(belongs)){
        belongs = current->elem == x;
        counter++;
        current = current->siguiente;
    }
    return belongs;
}

void AddS(int x, Set s){
    int counter = 1;
    NodoS* nuevo = new NodoS;
    nuevo->elem = x;
    nuevo->siguiente = NULL;
    NodoS* current = s->primero;
    while(counter<s->cantidad){
        counter++;
        current = current->siguiente;
    }
    current->siguiente = nuevo;
    s->cantidad++;
}

void RemoveS(int x, Set s){
    int counter = 1;
    NodoS* current = s->primero;
    while(counter<=s->cantidad){
        counter++;
        if(current->elem == x){
            NodoS* temp = current;
            current = current->siguiente;
            delete temp;
            s->cantidad--;
        }
        else{
            current = current->siguiente;
        }   
    }
}

int sizeS(Set s){
    return s->cantidad;
}

LinkedList setToList(Set s){
    int counter = 1;
    LinkedListSt* list = new LinkedListSt;
    NodoS* current = s->primero;
    while(counter<=s->cantidad){
        Snoc(list, current->elem);
        counter++;
        current = current->siguiente;
    }
    return list;
}

void DestroyS(Set s){
    delete s;
}



