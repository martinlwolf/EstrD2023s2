struct ArrayListSt {
    int cantidad;
    int *elementos;
    int capacidad;
}

typedef ArrayListSt* ArrayList

ArrayList newArrayList(){
    ArrayList newArray = new ArrayListSt;
    newArray -> cantidad = 0;
    newArray -> capacidad = 16;
    newArray -> elementos = new int[16];
    return newArray;
}

ArrayList newArrayListWith(int cap){
    ArrayList newArray = new ArrayListSt;
    newArray -> cantidad = 0;
    newArray -> capacidad = cap;
    newArray -> elementos = new int[cap];
    return newArray;
}

int lengthAL(ArrayList xs){
    return xs -> cantidad;
}

int get(int i, ArrayList xs){
    return xs -> elementos[i];
}

void set(int i, int x, ArrayList xs){
    xs -> elementos[i] = x;
}

void resize(int capacidad, ArrayList xs){
    int* elementosNuevo= new int[capacidad];
    for(int i=0; i < min(cap,xs->cantidad), i++){
        elementosNuevo[i]= xs -> elementos[i];
    }
    delete(xs -> elementos);
    xs -> elementos = elementosNuevo;
    xs -> capacidad = capacidad;
    xs -> cantidad = min(capacidad, xs-> cantidad);
}

void add(int x, ArrayList xs){
    if(xs -> cant == xs -> cap){
        duplicarCap(xs);
    }
    xs -> elementos[xs -> cantidad] = x;
    xs -> cantidad++;
}

void duplicarCap(ArrayList xs){
    int* elementosNuevo = new int[xs -> cap*2];
    for(int i=0; i<xs -> cantidad; i++){
        elementosNuevo[i] = xs->elementos[i];
    }
    delete(xs->elementos);
    xs->elementos = elementosNuevo;
    xs->cap *= 2;
}

void remove(ArrayList xs){
    if(xs->cantidad == 0){
        perror("ArrayVacio");
    }
    xs -> cantidad--;
}

int sumatoria(ArrayList xs){
    int acum = 0;
    for(int i = 0; i<lenghtAL(xs); i++){
        acum += get(i,xs);
    }
    return acum;
}
