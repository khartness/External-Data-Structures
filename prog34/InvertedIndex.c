InvertedIndex * openInvertedIndex(char * filename, int keyLength) {
	InvertedIndex * file = (InvertedIndex *)malloc(sizeof(InvertedIndex));
	u_int32_t flags;
	int ret;

	file->keySize = keyLength;
	file->current = 0;
	file->lastKey = (char *)malloc(keyLength);
	file->lastKey[0] = '\0';
	ret = db_create(&(file->invertedLists), NULL, 0);
	if (ret != 0) {
	    free(file);
	    return NULL;
	}
	/* Database open flags */
	flags = DB_CREATE; /* If the database does not exist, create it.*/
	/* open the database */
	ret = file->invertedLists->open(file->invertedLists, NULL, filename, NULL, DB_BTREE, flags, 0);
	if ( ret != 0 ) {
		free(file);
		return NULL;
	}
	return file;
}

int addToInvertedIndex(InvertedIndex * file, char * key, unsigned position) {
	DBT key, data;

	memset(&key, 0, sizeof(DBT));
	memset(&data, 0, sizeof(DBT));
   	key.data = file->lastKey;
   	key.size = file->keySize;
   	data.data = &(file->last);
   	data.ulen = sizeof(file->last);
   	data.flags = DB_DBT_USERMEM;
	if ( strcmp(file->lastKey, key) != 0 ) {
    	strncpy(file->lastKey, key, file->keySize);
    	if ( file->invertedLists->get(file->invertedLists, NULL, &key, &data, 0) == DB_NOTFOUND ) {
    		file->last.length = 1;
    		file->last.position[0] = position;
    		file->invertedLists->put(file->invertedLists, NULL, &key, &data, DB_NOOVERWRITE);
    	} else {
    		if ( file->last.length < 127 ) {
    			file->last.position[rec.length] = position;
    			file->last.length++;
    			file->invertedLists->put(file->invertedLists, NULL, &key, &data, 0);
    		} else {
    			return 0; /* this needs to be corrected to allow overflow */
    		}
		}
	} else {
   		if ( file->last.length < 127 ) {
   			file->last.position[rec.length] = position;
   			file->last.length++;
   			file->invertedLists->put(file->invertedLists, NULL, &key, &data, 0);
   		} else {
   			return 0; /* this needs to be corrected to allow overflow */
   		}
	}

	return 1;
}

unsigned findFirstInvertedIndex(InvertedIndex * file, char * key) {
	DBT key, data;

	memset(&key, 0, sizeof(DBT));
	memset(&data, 0, sizeof(DBT));
   	key.data = file->lastKey;
   	key.size = file->keySize;
   	data.data = &(file->last);
   	data.ulen = sizeof(file->last);
   	data.flags = DB_DBT_USERMEM;
	if ( strcmp(file->lastKey, key) != 0 ) {
    	strncpy(file->lastKey, key, file->keySize);
    	if ( file->invertedLists->get(file->invertedLists, NULL, &key, &data, 0) == DB_NOTFOUND )
    		return NO_RECORD;
	}
	file->current = 0;
	if ( file->last.length > 0 ) {
		file->current = 1;
	    return file->last.position[0];
	} else
		return NO_RECORD;
}

unsigned findNextInvertedIndex(InvertedIndex * file) {
	if ( file->current < file->last.length ) {
		return file->last.position[file->current++];
	else
		return NO_RECORD;
}

void closeInvertedIndex(InvertedIndex * file) {
	file->invertedLists->close(file->invertedLists, 0);
}

int findInvertedList(InvertedIndex * file, InvertedList * list) {
	if ( findFirstInvertedIndex(file, list->key) != NO_RECORD ) {
		int i;

		list->current = 0;
		list->length = file->last.length;
		list->position = (unsigned *)malloc(sizeof(unsigned) * list->length);
		for ( i = 0; i < list->length; ++i )
		    list->position[i] = file->last.position[i];
		return 1;
	}
	else
	    return 0;
}
