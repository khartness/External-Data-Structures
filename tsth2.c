#include <stdio.h>
#include <stdlib.h>

unsigned long h2(unsigned key, unsigned long tablesize) {
    unsigned long weights[10] = {37, 2, 19, 6, 31, 14, 3, 28, 7, 34};
    unsigned long pos = 0;
    int i;

    for ( i = 0; i < 4 || key > 0; ++i ) {
        unsigned digit = key % 10;
        pos = pos * 41 + weights[digit];
        key /= 10;
    }
    return pos % (tablesize - 1) + 1;
}

unsigned long h3(unsigned key, unsigned long tablesize) {
    unsigned long srcRadix = 11;
    unsigned long destRadix = 13;
    unsigned pos = 0;

    if ( tablesize == 12 || tablesize == 14 ) {
        srcRadix = 17;
        destRadix = 23;
    }

    while ( key > 0 ) {
        pos = destRadix * pos + key % srcRadix;
        key = key / srcRadix;
    }
    return pos % (tablesize - 1) + 1;
}

void report(char * title, int counts[]) {
	int i;
	printf("%40s\n\n", title);
	for ( i = 0; i < 10; ++i )
	    printf("     %2d: %4d\n", i+1, counts[i]);
	printf( "\n\n" );
}

int main() {
    int counts[10] = {0,0,0,0,0,0,0,0,0,0};
    unsigned samples[8] = {333, 632, 1090, 459, 238, 1982, 3411, 379};
    int i;

    for ( i = 0; i < 8; ++i )
        counts[h2(samples[i], 11)-1]++;
    report("Samples with h2", counts);
    for ( i = 0; i < 992; ++i )
        counts[h2(3412+i, 11)-1]++;
    report("Adding 3412-4403", counts);
    for ( i = 0; i < 10; ++i )
        counts[i] = 0;
    for ( i = 0; i < 8; ++i )
        counts[h3(samples[i], 11)-1]++;
    report("Samples with h3", counts);
    for ( i = 0; i < 992; ++i )
        counts[h3(3412+i, 11)-1]++;
    report("Adding 3412-4403", counts);
}
