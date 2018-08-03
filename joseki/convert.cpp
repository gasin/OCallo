#include <bits/stdc++.h>
using namespace std;

int main() {
    ifstream ifs("joseki.txt");
    ofstream ofs("../joseki.ml");

    string str[8];

    ofs << "open Int64;;" << endl;
    ofs << "open Hashtbl;;" << endl;
    ofs << "let joseki_table : ((int64 * int64), ((int*int))) Hashtbl.t = Hashtbl.create 1000;;" << endl;

    while (ifs >> str[0]) {
        cout << str[0] << endl;
        for (int i = 1; i < 8; i++) {
            ifs >> str[i];
            cout << str[i] << endl;
        }
        cout << endl;
        if (str[0][0] == 'x') continue;
        string black[4], white[4];
        int x[4], y[4];

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 64; j++) {
                black[i].push_back('0');
                white[i].push_back('0');
            }
        }

        for (int i = 0; i < 8; i++) {
            for (int j = 0; j < 8; j++) {
                if (str[i][j] == '0') {
                    white[0][i*8+j] = '1';
                    white[1][j*8+i] = '1';
                    white[2][(7-i)*8+(7-j)] = '1';
                    white[3][(7-j)*8+(7-i)] = '1';
                } else if (str[i][j] == '1') {
                    black[0][i*8+j] = '1';
                    black[1][j*8+i] = '1';
                    black[2][(7-i)*8+(7-j)] = '1';
                    black[3][(7-j)*8+(7-i)] = '1';
                } else if (str[i][j] == '2') {
                    x[0] = 7-i;
                    x[1] = 7-j;
                    x[2] = i;
                    x[3] = j;

                    y[0] = 7-j;
                    y[1] = 7-i;
                    y[2] = j;
                    y[3] = i;
                }
            }
        }
        for (int i = 0; i < 4; i++) {
            ofs << "Hashtbl.add joseki_table (0b" << black[i] << "L,0b" << white[i] << "L) (" << x[i] << "," << y[i] << ");;" << endl;
        }
    }
}
