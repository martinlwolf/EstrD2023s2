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

Tree nodeT(int x, Tree left, Tree right){
    NodeT* tree = new NodeT;
    tree->elem = x;
    tree->left = left;
    tree->right = right;

}

bool isEmptyT(Tree t){
    return t->elem == NULL;
}

int rootT(Tree t){
    return t->elem;
}

Tree left(Tree t){
    return t->left;
}

Tree right(Tree t){
    return t->right;
}