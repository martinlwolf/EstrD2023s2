#include <iostream>
#include <Persona.h>
using namespace std;

struct PersonaSt{
    string nombre;
    int edad;
};

Persona consPersona(string nombre, int edad){
    PersonaSt* newPersona = new PersonaSt;
    newPersona->edad = edad;
    newPersona->nombre = nombre;
    return newPersona;
};

string nombre(Persona p){
    return p->nombre;
};

int edad(Persona p){
    return p->edad;
}

void crecer(Persona p){
    p->edad+=1;
}

void cambioDeNombre(string nombre, Persona p){
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2){
    p1->edad > p2->edad;
}

Persona laQueEsMayor(Persona p1, Persona p2){
    if(p1->edad > p2->edad){
        return p1;
    }
    else return p2;
}



    

