#include <iostream>
#include <Pokemon.h>
using namespace std;

struct PokeSt {
    TipoDePokemon tipo;
    int vida;
};

Pokemon consPokemon(TipoDePokemon tipo){
    PokeSt* newPok = new PokeSt;
    newPok->vida;
    return newPok;
}

TipoDePokemon tipoDePokemon(Pokemon p){
    return p->tipo;
}

int energia(Pokemon p){
    return p->vida;
}

void perderEnergia(int energia, Pokemon p){
    p->vida -= energia;
}

bool superaA(Pokemon p1, Pokemon p2){
    if(p1->tipo == "Agua" && p2->tipo == "Fuego"){
        return true;
    }
    else if (p1->tipo == "Fuego" && p2->tipo == "Planta"){
        return true;
    }
    else if (p1->tipo == "Planta" && p2->tipo == "Agua")
    {
        return true;
    }
    else return false;
    
}
