package org.snt.cnetworkparser.lang.sol;


public class StringPair {

    public StringPair(String key ,String value) {
        this.key = key;
        this.value = value;
    }

    public StringPair() {}

    public String key;
    public String value;

    public String toString() {
        return "key: " + key + " value: "+ value;
    }
}