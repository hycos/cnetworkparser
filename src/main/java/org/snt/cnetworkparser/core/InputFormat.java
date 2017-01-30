package org.snt.cnetworkparser.core;


import org.snt.cnetworkparser.lang.smt.CVC4Listener;
import org.snt.cnetworkparser.lang.smt.S3Listener;
import org.snt.cnetworkparser.lang.smt.Z3Str2Listener;
import org.snt.cnetworkparser.lang.sol.SolListener;

public enum InputFormat {

    SOL(0, "Sol","Sol.g4"),
    CVC4 (1, "CVC4", "CVC4.g4"),
    Z3STR2(2, "Z3Str2", "Z3Str2.g4"),
    S3(3, "S3", "S3.g4");


    private int id;
    private String name;
    private String grammar;

    InputFormat(int id, String name, String grammar) {
        this.id = id;
        this.name = name;
        this.grammar = grammar;
    }

    public String getGrammar() {
        return this.grammar;
    }

    public String getName() {
        return this.name;
    }

    public static InputFormat getFormat(String desc) {
        switch(desc) {
            case "sol" : return SOL;
            case "cvc4" : return CVC4;
            case "z3str2": return Z3STR2;
            case "s3" : return S3;
        }
        return null;
    }

    public ConstraintNetworkProvider getProvider(){
        return getProvider(false);
    }

    public ConstraintNetworkProvider getProvider(boolean eufEnabled) {
        switch (this) {
            case SOL: return new SolListener(eufEnabled);
            case CVC4: return new CVC4Listener(eufEnabled);
            case Z3STR2: return new Z3Str2Listener(eufEnabled);
            case S3: return new S3Listener(eufEnabled);
        }
        return null;
    }

    public static String getAvailableFormats() {
        return "(" + SOL.getName() + "," + CVC4.getName() + "," + Z3STR2.getName() + "," + S3.getName() +")";
    }



}
