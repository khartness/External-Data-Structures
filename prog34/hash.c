#include <stdlib.h>

unsigned hash(char * key) {
	unsigned result = 87383;
	int len = strlen(key);
	unsigned last4;
	unsigned keyValue;

	if ( len < 4 )
	    last4 = atoi(key);
	else
	    last4 = atoi(key + len - 4);

	if ( len < 10 )
	    keyValue = atoi(key);
	else {
		keyValue = atoi(key+1);
		result = (result * 257 + keyValue % 353) % 429497;
		keyValue = keyValue / 353 + (key[0] - '0') * 2832861;
	}

	while ( keyValue != 0 ) {
		result = (result * 257 + keyValue % 353) % 429497;
		keyValue /= 353;
	}

	return result + last4 * 429497;
}
