package edu.shsu.hartness.records;

public class HeaderRecord {
    private byte[] signature;
    private unsigned int fileSize;
    private unsigned int headOfDeleted;
    private int recordSize;
    private byte[] matchingSignature;
    private static final int INT_SIZE = Integer.size() / Byte.size();
    private static final int MINHEADERSIZE = 2 * INT_SIZE;

    public HeaderRecord(int recsize, byte[] desiredSignature) {
        recordSize = recsize;
        matchingSignature = desiredSignature;
        if ( recordSize < 1 )
            throw new RecordIOException("Illegal record size");
        else if ( recordSize < MINHEADERSIZE )
            if ( MINHEADERSIZE % recordSize == 0 )
                recordSize = MINHEADERSIZE;
            else
                recordSize = (MINHEADERSIZE/recordSize + 1) * recordSize;

        if ( recordSize > MINHEADERSIZE ) {
            signature = new byte[recordSize - MINHEADERSIZE];
            for ( int i = 0; i < signature.length; ++i )
                if ( i < matchingSignature.length )
                    signature[i] = matchingSignature[i];
                else
                    signature[i] = 0;
        } else
            signature = null;

        fileSize = 0;
        headOfDeleted = 0;
    }

    public void read(RandomAccessFile file) throws IOException {
        file.seek(0);
        fileSize = file.readInt();
        try {
            headOfDeleted = file.readInt();
            file.readFully(signature);
            for ( int i = 0; i < signature.length && i < matchingSignature.length; ++i )
                if ( signature[i] != matchingSignature[i] )
                    throw new RecordIOException("Header signature doesn't match");
		} catch ( EOFException e ) {
			throw new RecordIOException("Corrupt header", e);
		}
    }

    public void write(RandomAccessFile file) throws IOException {
		file.seek(0);
		if ( fileSize == 0 )
		    fileSize = 1;
		file.writeInt(fileSize);
		file.writeInt(headOfDeleted);
		file.write(signature);
    }

    public unsigned int numberOfRecords() {
        return fileSize;
    }

    public void extendFile() {
		++fileSize;
	}

    public unsigned int deletedList() {
        return headOfDeleted;
    }

    public void setDeletedList(unsigned int recordOffset) {
        headOfDeleted = recordOffset;
    }
}
