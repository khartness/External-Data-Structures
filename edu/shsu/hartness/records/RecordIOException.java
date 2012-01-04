package edu.shsu.hartness.records;

public class RecordIOException extends Exception {
    public RecordIOException() {
        super("edu.shsu.hartness.records I/O exception");
    }
    
    public RecordIOException(String msg) {
        super(msg);
    }
    
    public RecordIOException(String msg, Throwable e) {
        super(msg, e);
    }
}
