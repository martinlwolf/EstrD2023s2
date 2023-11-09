#include <iostream>
#include <Entrenador.h>
#include <tp9.cpp>
using namespace std;

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    EntrenadorSt* newEnt = new EntrenadorSt;
    newEnt->nombre = nombre;
    newEnt->pokemon = new Pokemon[cantidad];
    newEnt->cantPokemon = cantidad;
    return newEnt;
}

string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

int cantidadDePokemon(Entrenador e){
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int count = 0;
    for(int i=0; i < e->cantPokemon; i++){
        count += unoSi0Sino(e->pokemon[i]->tipo == tipo);
    }
    return count;
}

Pokemon pokemonNro(int i, Entrenador e){
    return e->pokemon[i];
}

bool leGanaATodos(Entrenador e1, Entrenador e2){
    bool leGana = false;
    int index = 0;
    while(not(leGana) && index < e1->cantPokemon){
        leGana = pokLesGanaATodos(e1->pokemon[index], e2);
        index++;
    }
    return leGana;
}

bool pokLesGanaATodos(Pokemon pok, Entrenador e){
    bool lesGana = true;
    int index = 0;
    while(lesGana && index < e->cantPokemon){
        lesGana = superaA(pok, e->pokemon[index]);
        index++;
    }
    return lesGana;
}



