#if ! defined(_CUSTOMER_H)
#define _CUSTOMER_H 1

typedef
    struct s_Customer {
        char ID[8];
        char name[21];
        double balance;
    }
    Customer;
#endif
