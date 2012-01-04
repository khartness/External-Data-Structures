// transprimes.cpp
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    ifstream in("primes.txt");
    ofstream out("primes.c");
    int count = 1;
    int number;

    out << "static unsigned primes[10000] = { ";
    if ( in >> number )
        out << number;
    while ( in >> number ) {
        if ( ++count % 20 == 0 )
            out << ",\n";
        else
            out << ", ";
        out << number;
    }
    out << "};\n";
    in.close();
    out.close();
    return 0;
}
