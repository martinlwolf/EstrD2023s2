#include <iostream>
#include "Tree.cpp"
#include "LinkedList.cpp"
using namespace std;


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

//EJERCICIO 7
int sumarT(Tree t){
    if(!isEmptyT(t)){
        return (t->elem + sumarT(t->left) + sumarT(t->right));
    }
    else{
        return 0;
    }
}

int sizeT(Tree t){
    if(!isEmptyT(t)){
        return(1 + sizeT(t->left) + sizeT(t->right));
    }
    else{
        return 0;
    }
}

bool perteneceT(int e, Tree t){
     if(!isEmptyT(t)){
        return((t->elem == e) || perteneceT(e, t->left) || perteneceT(e, t->right));
     }
     else{
        false;
     }
}

int aparicionesT(int e, Tree t){
    if(!isEmptyT(t)){
        return unoSi(t->elem == e) + aparicionesT(e, t->left) + aparicionesT(e, t->right);
    }
    else{
        return 0;
    }
}

int heightT(Tree t){
    if(!isEmptyT(t)){
        return(1 + max(heightT(t->left), heightT(t->left)));
    }
}

LinkedList toList(Tree t){
    LinkedListSt* lista = emptyList();
    Cons(t->elem, lista);
    Append(toList(t->left), lista);
    Append(toList(t->right), lista);
    return lista;
}

LinkedList leaves(Tree t){
    LinkedListSt* lista = emptyList();
    if(!isEmptyT(t)){
    if(isEmptyT(t->left) && isEmptyT(t->right)){
        Cons(t->elem, lista);
    }
    else{
        Append(lista, leaves(t->left));
        Append(lista, leaves(t->left));
    }
    }
    else{
        return emptyList();
    }
}

LinkedList levelN(int n, Tree t){
    if(!isEmptyT(t)){
        if(n != 0){
            LinkedList list = levelN(n-1, t->left);
            Append(list, levelN(n-1, t->right));
            return list;
        }
        else{
            LinkedList list = emptyList();
            Cons(t->elem, list);
            return (list);
        }
    }
    else{
        return emptyList();
    }
}


int main(){
    cout << "a" << endl;
}

int unoSi(bool cond){
    if(cond){
        return 1;
    }
    else{
        return 0;
    }
}