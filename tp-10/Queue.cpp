#include <iostream>
#include "Queue.h"
#include "LinkedList.cpp"
using namespace std;


struct NodoQ {
int elem; // valor del nodo
NodoQ* siguiente; // puntero al siguiente nodo
};
struct QueueSt {
int cantidad; // cantidad de elementos
NodoQ* primero; // puntero al primer nodo
NodoQ* ultimo; // puntero al ultimo nodo
};

Queue emptyQ(){
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
}

bool isEmptyQ(Queue q){
    return q->cantidad == 0;
}

int firstQ(Queue q){
    q->primero;
}

void Enqueue(int x, Queue q){
    NodoQ* nNodo = new NodoQ;
    nNodo->elem = x;
    nNodo->siguiente = NULL;
    q->ultimo->siguiente = nNodo;
    q->ultimo = nNodo;
}

void Dequeue(Queue q){
    NodoQ* temp = q->primero; //PREGUNTAR SI SE BORRA EL PUNTERO O EL NODO//
    q->primero = q->primero->siguiente;
    delete temp;
}

int lengthQ(Queue q){
    q->cantidad;
}

void MergeQ(Queue q1, Queue q2){
    q1->ultimo->siguiente = q2->primero;
    q1->cantidad += q2->cantidad;
    q1->ultimo = q2->ultimo;
    delete q2;
}