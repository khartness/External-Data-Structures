#include "primes.h"
#include <stdio.h>

int main() {
    int number = 1;
    
    while ( number > 0 ) {
        printf("Enter number (0 to quit): ");
        scanf("%d", &number);
        printf("%10d\t%10d\n", number, nextPrime(number));
    }
    return 0;
}
