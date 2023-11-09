#include <iostream>
#include <ArrayList.cpp>
using namespace std;

//EJERCICIO 1



//EJERCICIO 4


int sumatoria(ArrayList xs){
    int acum = 0;
    for(int i = 0; i<lengthAL(xs); i++){
        acum += get(i,xs);
    }
    return acum;
}

void sucesores(ArrayList xs){
    for(int i = 0; i<lengthAL(xs);i++){
        set(i, get(i,xs)+1,xs);
    }
}

bool pertenece(int x, ArrayList xs){
    int count = 0;
    bool prtnc = false;
    while(count < lengthAL(xs)){
        prtnc = get(count, xs);
        count++;
    }
    return prtnc;
}

int apariciones(int x, ArrayList xs){
    int count = 0;
    int restantes = (lengthAL(xs)-1);
    while(restantes >= 0){
        count+= unoSi0Sino(xs->elementos[restantes]);
        restantes--;
    }
    return count;
}

int minimo(ArrayList xs){
    int minActual = xs->elementos[0];
    int index = 0;
    while(index < lengthAL(xs)){
        minActual = min(minActual, xs->elementos[index]);
    }
    return minActual;
}

int unoSi0Sino(bool cond){
    if(cond){
        return 1;
    }
    else return 0;
}