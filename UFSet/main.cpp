#include <iostream>
#include <vector>
#include "Equipo.h"
#include "Test.h"
#include "UFSet.cpp"

using namespace std;

int main()
{
    /*cout << "Inicializando equipos..." << endl;

    Equipo estadosUnidos = crearEquipo(ESTADOSUNIDOS, "A");
    Equipo costaRica     = crearEquipo(COSTARICA,     "A");
    Equipo colombia      = crearEquipo(COLOMBIA,      "A");
    Equipo paraguay      = crearEquipo(PARAGUAY,      "A");

    Equipo haiti         = crearEquipo(HAITI,         "B");
    Equipo peru          = crearEquipo(PERU,          "B");
    Equipo brasil        = crearEquipo(BRASIL,        "B");
    Equipo ecuador       = crearEquipo(ECUADOR,       "B");

    Equipo venezuela     = crearEquipo(VENEZUELA,     "C");
    Equipo mexico        = crearEquipo(MEXICO,        "C");
    Equipo uruguay       = crearEquipo(URUGUAY,       "C");
    Equipo jamaica       = crearEquipo(JAMAICA,       "C");

    Equipo argentina     = crearEquipo(ARGENTINA,     "D");
    Equipo bolivia       = crearEquipo(BOLIVIA,       "D");
    Equipo chile         = crearEquipo(CHILE,         "D");
    Equipo panama        = crearEquipo(PANAMA,        "D");

    cout << "Probando UFSet sobre equipos de la Copa..." << endl << endl;

    // Test 1
    vector<Equipo> caso1;
    caso1.push_back(argentina);
    caso1.push_back(chile);
    caso1.push_back(venezuela);
    caso1.push_back(colombia);
    caso1.push_back(paraguay);
    caso1.push_back(mexico);
    caso1.push_back(uruguay);
    test("Caso 1", caso1);

    // Test 2
    vector<Equipo> caso2;
    caso2.push_back(estadosUnidos);
    caso2.push_back(brasil);
    caso2.push_back(jamaica);
    caso2.push_back(argentina);
    caso2.push_back(uruguay);
    test("Caso 2", caso2);

    // Test 3
    vector<Equipo> caso3;
    caso3.push_back(estadosUnidos);
    caso3.push_back(costaRica);
    caso3.push_back(colombia);
    caso3.push_back(paraguay);

    caso3.push_back(haiti);
    caso3.push_back(peru);
    caso3.push_back(brasil);
    caso3.push_back(ecuador);

    caso3.push_back(venezuela);
    caso3.push_back(mexico);
    caso3.push_back(uruguay);
    caso3.push_back(jamaica);

    caso3.push_back(argentina);
    caso3.push_back(bolivia);
    caso3.push_back(chile);
    caso3.push_back(panama);
    test("Caso 3", caso3);*/

    Equipo peru          = crearEquipo(PERU,          "B");
    Equipo brasil        = crearEquipo(BRASIL,        "B");
    Equipo ecuador       = crearEquipo(ECUADOR,       "B");
    Equipo haiti         = crearEquipo(HAITI,         "B");
    UFSet p = createUFS(peru);
    UFSet b = createUFS(brasil);
    UFSet e = createUFS(ecuador);
    UFSet h = createUFS(haiti);
    cout << nombreEquipo(elemUFS(p)) << endl;
    cout << nombreEquipo(elemUFS(b)) << endl;
    cout << nombreEquipo(elemUFS(e)) << endl;
    cout << nombreEquipo(elemUFS(h)) << endl;
    cout << nombreEquipo(elemUFS(findUFS(p))) << endl;
    cout << nombreEquipo(elemUFS(findUFS(b))) << endl;
    cout << nombreEquipo(elemUFS(findUFS(e))) << endl;
    cout << nombreEquipo(elemUFS(findUFS(h))) << endl;
    cout << p->rango << endl;
    cout << b->rango << endl;
    cout << e->rango << endl;
    cout << h->rango << endl;
    unionUFS(p,b);
    /*cout << nombreEquipo(elemUFS(findUFS(p))) << endl;
    cout << nombreEquipo(elemUFS(findUFS(b))) << endl;
    cout << nombreEquipo(elemUFS(findUFS(e))) << endl;
    cout << nombreEquipo(elemUFS(findUFS(h))) << endl;
    cout << p->rango << endl;
    cout << b->rango << endl;
    cout << e->rango << endl;
    cout << h->rango << endl;*/
    unionUFS(e, h);
    cout << nombreEquipo(elemUFS(findUFS(p))) << endl;
    cout << nombreEquipo(elemUFS(findUFS(b))) << endl;
    cout << nombreEquipo(elemUFS(findUFS(e))) << endl;
    cout << nombreEquipo(elemUFS(findUFS(h))) << endl;
    cout << nombreEquipo(elemUFS(p->padre)) << endl;
    cout << nombreEquipo(elemUFS(b->padre)) << endl;
    cout << nombreEquipo(elemUFS(e->padre)) << endl;
    cout << nombreEquipo(elemUFS(h->padre)) << endl;
    cout << p->rango << endl;
    cout << b->rango << endl;
    cout << e->rango << endl;
    cout << h->rango << endl;
    unionUFS(p, e);
    //cout << nombreEquipo(elemUFS(findUFS(p))) << endl;
    //cout << nombreEquipo(elemUFS(findUFS(b))) << endl;
    //cout << nombreEquipo(elemUFS(findUFS(e))) << endl;
    cout << nombreEquipo(elemUFS(findUFS(h))) << endl;
    cout << nombreEquipo(elemUFS(p->padre)) << endl;
    cout << nombreEquipo(elemUFS(b->padre)) << endl;
    cout << nombreEquipo(elemUFS(e->padre)) << endl;
    cout << nombreEquipo(elemUFS(h->padre)) << endl;
    cout << p->rango << endl;
    cout << b->rango << endl;
    cout << e->rango << endl;
    cout << h->rango << endl;

    return 0;
}
