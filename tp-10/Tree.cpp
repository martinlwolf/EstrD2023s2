#include <iostream>
#include "Tree.h"
using namespace std;

struct NodeT {
int elem;
NodeT* left;
NodeT* right;
};

Tree emptyT(){
    NodeT* tree = new NodeT;
    tree->elem = NULL;
    tree->left = emptyT();
    tree->right = emptyT();
}