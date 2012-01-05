package edu.shsu.hartness.records;

import java.io.EOFException;
import java.io.IOException;
import java.io.RandomAccessFile;

/**
 * DirectIO
 * A RandomAccessFile with additional methods to support handling the file as
 * a file of objects. The file is presumed to begin with a header consisting
 * of one or more "records" and including the number of records in the file
 * and a reference to the head of a list of "deleted" records.
 * Records large enough to contain one object, each, are allocated at the end
 * of the file or removed from the deleted list.
 * NOTE: My C mentality has caused me to make incorrect assumptions about Java's
 * ability to recognize space requirements for instances of a class, so this is
 * definitely unfinished code.
 * @param <T> Object type of record.
 * @author Ken T. N. Hartness (c) 2011
 * @version %I%, %G%
 * @see java.io.RandomAccessFile
 **/
public class DirectIO<T> extends RandomAccessFile {
	private HeaderRecord hdr;
    public final static int recordSize = T.size() / Byte.size();

    public DirectFile(String filename) throws IOException {
		super(filename, "rws");
		hdr = new HeaderRecord();
		try {
    		hdr.read(this);
		} catch ( EOFException e ) {
			hdr.write(this);
		}
	}

    public void open(String filename) throws IOException {
		super.open(filename);
		try {
    		hdr.read(this);
		} catch ( EOFException e ) {
			hdr = new RecordHeader(); // make sure we aren't using an old header
			hdr.write(this);
		}
    }

    public void close() throws IOException {
		hdr.write(this);
		super.close();
    }

    public void flushHeader() throws IOException {
		hdr.write(this);
	}

    public unsigned int allocateRecord() throws IOException {
        unsigned int ptr = hdr.deletedList();
        if ( ptr == 0 ) {
			ptr = hdr.numberOfRecords * recordSize;
			seek(ptr);
			byte[] buffer = new byte[recordSize];
			for ( int i = 0; i < recordSize; ++i )
			    buffer[i] = 0;
			write(buffer);
			hdr.extendFile();
		} else {
			seek(ptr);
			hdr.setDeletedList(readInt());
		}
		seek(ptr);
		return ptr / recordSize;
    }

    public void deallocateRecord(unsigned int ID) throws IOException {
		unsigned int ptr = ID * recordSize;
		seek(ptr);
		if ( recordSize < 4 ) {
			byte[] buffer = new byte[recordSize];
			for ( int i = 0; i < recordSize; ++i )
			    buffer[i] = 0;
			write(buffer);
		} else {
		    writeInt(hdr.deletedList());
		    hdr.setDeletedList(ptr);
		}
	}

    public T read() throws IOException {
		Object obj;
		byte[] buffer = new byte[recordSize];
		try {
			readFully(buffer);
			ByteArrayInputStream bis = new ByteArrayInputStream(buffer);
			ObjectInputStream ois = new ObjectInputStream(bis);
			obj = ois.readObject();
		} catch ( EOFException e ) {
			return null;
		}
		return (T)obj;
    }

    public T read(unsigned int id) throws IOException {
		seek(id * recordSize);
		return read();
	}

    public void write(T obj) throws IOException, RecordIOException {
        byte[] buffer;
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try {
            ObjectOutputStream oos = new ObjectOutputStream(bos);
            oos.writeObject(obj);
            oos.flush();
            oos.close();
            bos.close();
            buffer = bos.toByteArray();
        } catch ( IOException e ) {
            throw new RecordIOException("Failure to format object in DirectIO.write", e);
        }
        write(buffer);
    }

    public void write(unsigned int id, T obj) throws IOException, RecordIOException {
		seek(id * recordSize);
		write(obj);
	}

	public unsigned int size() {
		return hdr.numberOfRecords();
	}
}
