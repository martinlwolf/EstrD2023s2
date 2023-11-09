#include <iostream>
using namespace std;
#include "LinkedList.cpp"

int sumatoria(LinkedList xs){
    int sumatoria = 0;
    ListIterator ixs = getIterator(xs);
    while(not(atEnd(ixs))){
        sumatoria += ixs->current->elem;
        next(ixs);
    };
    return sumatoria;
}
//--O(N) siendo n el total de elementos recorridos de la lista 

void Sucesores(LinkedList xs){
    LinkedListSt* newList = new LinkedListSt;
    ListIterator ixs = getIterator(xs);
    while(not(atEnd(ixs))){
        Cons(ixs->current->elem + 1, newList);
        next(ixs);
    };
}
//--O(N) siendo n el total de elementos recorridos de la lista 

bool pertenece(int x, LinkedList xs){
    bool pert = false;
    ListIterator ixs = getIterator(xs);
    while(not(atEnd(ixs)) && not(pert)){
        pert = ixs->current->elem == x;
        next(ixs);
    };
    return pert;
}
//--O(N) siendo n el total de elementos recorridos de la lista 

int apariciones(int x, LinkedList xs){
    int apars = 0;
    ListIterator ixs = getIterator(xs);
    while(not(atEnd(ixs))){
        if(ixs->current->elem == x){
            apars++;
        }
        next(ixs);
    }
    return apars;
}
//--O(N) siendo n el total de elementos recorridos de la lista 

int minimo(LinkedList xs){
    int minimoHastaAhora = xs->primero->elem;
    ListIterator ixs = getIterator(xs);
    while(not(atEnd(ixs))){
        min(ixs->current->elem, minimoHastaAhora);
        next(ixs);
    }
    return minimoHastaAhora;
}

//--O(N) siendo n el total de elementos recorridos de la lista 

LinkedList copy(LinkedList xs){
    LinkedList copia = emptyList();
    ListIterator ixs = getIterator(xs);
    while(not(atEnd(ixs))){
        Snoc(copia, ixs->current->elem);
        next(ixs);
    }
    return copia;
}

void Append(LinkedList xs, LinkedList ys){
    ListIterator iys = getIterator(ys);
    while(not(atEnd(iys))){
        Snoc(xs, iys->current->elem);
        next(iys);
    }
    delete ys;
}


int main(){
    cout << "a" << endl;
}
