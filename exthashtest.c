#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "primes.h"

unsigned h(char *);
void add(char *);
void report(int, int);

int main() {
    char name[30];
    int power;

    printf("Enter name: ");
    gets(name);
    while ( strcmp(name, "done") != 0 ) {
        printf("%d: %s\n", h(name), name);
        add(name);
        printf("Enter name (or done): ");
        gets(name);
    }

    power = 2;
    while ( power <= 64 ) {
        report(power, 3);
        power = power * 2;
    }
    return 0;
}

unsigned h(char * str) {
    static unsigned radix = 0;
    static unsigned maxTable = 0;
    int i, pastEnd;
    unsigned result = 0;

    if ( radix == 0 ) {
        radix = nextPrime(256);
        maxTable = nextPrime(0x7fffffff);
    }

    pastEnd = 0;
    for ( i = 0; i < 30; ++i ) {
        if ( str[i] == '\0' )
            pastEnd = 1;
        if ( pastEnd )
            result = (result * radix + 32) % maxTable;
        else
            result = (result * radix + str[i]) % maxTable;
    }

    return result;
}

char list[1000][30];
int numNames = 0;

void add(char * name) {
    strncpy(list[numNames], name, 30);
    ++numNames;
}

void report(int tablesize, int bucketSize) {
    int *counts = (int*)(malloc(sizeof(int)*tablesize));
    int i;

    for ( i = 0; i < tablesize; ++i )
        counts[i] = 0;

    printf("\nTable size: %d\n", tablesize);
    for ( i = 0; i < numNames; ++i ) {
        unsigned pos = h(list[i]) % tablesize;
        printf("%d: %s\n", pos, list[i]);
        if ( ++counts[pos] > bucketSize )
            break;
    }
    free(counts);
}
