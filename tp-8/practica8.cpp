void printFromTo(char c1, char c2) {
for(int i = 0; c1 + i <= c2; i++) {
cout << c1 + i << ", ";
}
cout << endl;
}

int main(){
    printFromTo(char 'a', char 'b');
}
