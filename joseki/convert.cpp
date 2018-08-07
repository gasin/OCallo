#include <bits/stdc++.h>
using namespace std;

int main() {
    ifstream ifs("xxx.gam");
    ofstream ofs("xxx_comp.gam");

    string str1, str2, str3;
    int skip = 0;
    while (ifs >> str1 >> str2 >> str3) {
        skip++;
        if (skip%4) continue;
        int cnt = 0;
        int val = 0;
        int len = 0;
        for (int i = 0; i < str1.size(); i++) {
            if (str1[i] != '-' && str1[i] != '+' && str1[i] != ':') {
                cnt++;
                if (cnt == 1) {
                    val += str1[i]-'a';
                } else {
                    val = val*8 + str1[i]-'1';
                    len++;
                    ofs << (char)(33+val);
                }
            } else {
                cnt = 0;
                val = 0;
            }
            if (len > 40) break;
        }
        ofs << endl;
    }
}
