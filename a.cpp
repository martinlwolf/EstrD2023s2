Par swap (Par p){
    int temp = p.x;
    p.x = p.y;
    p.y = temp;
    return p
}

bool pertenece (char c, string s){
    if (s.lenght() == 0){
        return false
    }
    else{
        s[0] == c || pertenece(c, s.substr(1,s.length()))
    }
}

bool perteneceI (char c, string s){
    res = false;
    for(int i = 0; i < s.length(); i++){
        res = res || c==s[i];
    }

    return res;
}

bool perteneceIConIf (char c, string s){
    for(int i = 0; i < s.length(); i++){
        if(s[i]==c){
            return true
        }
    }
    return false;
}