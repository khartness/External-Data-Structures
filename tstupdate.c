#include <stdio.h>
#include "updatefile.h"

/* prototypes for later functions */
void userInput(char *, char *, int);
void userInputDouble(char *, double *);

int main() {
    Customer cust;
    char ID[8];
    char filename[80];
    char intermediateFilename[84];

    userInput("Enter filename: ", filename, 80);
    strcpy(intermediateFilename, filename);
    strcat(intermediateFilename, ".bak");
    if ( ! openUpdateFile(filename, intermediateFilename) ) {
        fprintf(stderr, "Unable to open %s\n", filename);
        return 1;
    }

    userInput("Customer ID (or enter \"quit\"): ", ID, 8);
    while ( strcmp(ID, "quit") != 0 ) {
        if ( getRecord(ID, &cust) ) {
            char operation[2];
            double amount;

            printf("%s (%s), Balance of $%.2f\n", cust.name, cust.ID, cust.balance);
            userInput("Charge, Payment, or No change (C, P, or N): ", operation, 2);
            if ( operation[0] != 'n' && operation[0] != 'N' ) {
                userInputDouble("Enter amount: ", &amount);
                if ( operation[0] == 'c' || operation[0] == 'C' )
                    cust.balance += amount;
                else
                    cust.balance -= amount;
                updateRecord(&cust);
            }
        } else {
            printf("Adding %s...\n", ID);
            strcpy(cust.ID, ID);
            userInput("Name: ", cust.name, 21);
            userInputDouble("Balance: ", &cust.balance);
            insertRecord(&cust);
        }
        userInput("Customer ID (or enter \"quit\"): ", ID, 8);
    }

    closeUpdateFile();
    return 0;
}

void userInput(char * prompt, char * response, int size) {
    char ch;
    int pos = 0;
    printf(prompt);
    ch = getchar();
    if ( ch == '\n' )
        ch = getchar();
    while ( ch != '\n' && pos < size-1 ) {
        response[pos] = ch;
        ch = getchar();
        ++pos;
    }
    response[pos] = '\0';
}

void userInputDouble(char * prompt, double * response) {
    printf(prompt);
    scanf("%lf", response);
    while ( getchar() != '\n' )  /* skip past Enter/Return */
        ;
}
