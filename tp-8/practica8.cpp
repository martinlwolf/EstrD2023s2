#include <Par.h>
#include <iostream>
using namespace std;

int main(){
    cout << fst(consPar(1,2)) << endl;
}

/*EJERCICIO 3*/
Par consPar(int x, int y){
    Par p;
    p.x = x;
    p.y = y;
}

int fst(Par p){
    return p.x;
}

int snd(Par p){
    return p.y;
}

int maxDelPar(Par p){
    if(p.x > p.y){
        return p.x;
    }
    else return p.y;
}

Par swap(Par p){
    int temp = p.x;
    p.x = p.y;
    p.y = temp;
    return p;
}

Par divisionYResto(int n, int m){
    consPar((div(n,m).quot), (div(n,m).rem));
}

//EJERCICIO 4

void printN(int n, string s){
    int counter = 0;
    while (counter < n){
        cout << s << endl;
        counter++;
    }
}

void printNR(int n, string s){
    if(n >= 1){
        cout << s << endl ;
        printNR((n-1), s);
    }
}

void cuentaRegresiva(int n){
    while(n >= 0){
        cout << n << "\n";
        n--;
    }
}

void cuentaRegresivaR(int n){
    if(n >= 0){
        cout << n << "\n";
        cuentaRegresivaR(n-1);
    }
}

void desdeCeroHastaN(int n){
    int count = 0;
    while(count < n){
        cout << count << "\n";
        count++;
    }
}

void desdeCeroHastaNR(int n){
    if(n > 0){
        desdeCeroHastaN(n-1);
        cout << n << endl;
    }
    else 0;
}

int mult(int n, int m){
    while(n>1){
        m += m;
        n--;
    }
}

int multR(int n, int m){
    if(n > 1){
        m + multR((n-1), m);
    }
    else 0;
}

void primerosN(int n, string s){
    for(int i = 0; i<n ; i++){
        cout << s[i] << endl;
    }
}

void primerosNR(int n, string s){
    if(n > 0){
        cout << s[0] << endl;
        primerosNR((n-1), s.substr(1,s.length()));
    }
}

bool pertenece (char c, string s){
    string copia = s;
    while((s[0]!= c) && s.length() > 1){
        copia = copia.substr(1,copia.length());
    }
    return copia[0] == c;
}

bool perteneceR (char c, string s){
    if (s.length() == 0){
        return false;
    }
    else{
        s[0] == c || pertenece(c, s.substr(1,s.length()));
    }
}

int apariciones(char c, string s){
    int aps = 0;
    string copiaS = s;
    while(copiaS.length() > 0){
        if(c == s[0]){
            aps++;
        }
    copiaS = copiaS.substr(1,copiaS.length());
    }
}

int aparicionesR(char c, string s){
    if(s.length() > 0){
        if(c == s[0]){
            1 + aparicionesR(c, s.substr(1,s.length()));
        }
        else aparicionesR(c, s.substr(1,s.length()));
    }
    else 0;
}

//EJERCICIO 5
// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int n, int d){
    Fraccion f;
    f.numerador = n;
    f.denominador = d;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f){
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f){
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return f.numerador / f.denominador;
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2){
    return consFraccion((f1.numerador * f2.numerador), (f1.denominador * f2.denominador));
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p){
    if((p.numerador % p.denominador) == 0){
        return consFraccion((p.numerador / p.denominador), (p.denominador / p.denominador));
    }
    else{
        if((p.denominador % p.numerador) == 0){
             return consFraccion((p.numerador / p.numerador), (p.denominador / p.numerador));
        }
        else return p;
    }
}