package org.snt.cnetworkparser.lang.smt;

public enum LanguageElements {
    ITE("ITE"),
    // Regular expressions
    KSTAR("KSTAR"),
    KPLUS("KPLUS"),
    RCONCAT("RCONCAT"),
    UNION("UNION"),
    OPT("OPT"),
    RAN("RAN"),
    CONV("CONV"),
    MATCHES("MATCHES"),
    // Strings
    LEN("LEN"),
    CONTAINS("CONTAINS"),
    INDEXOF("INDEXOF"),
    PREFIXOF("PREFIXOF"),
    SUFFIXOF("SUFFIXOF"),
    SCONCAT("SCONCAT"),
    SUBSTRNG("SUBSTRING"),
    // Numeric
    EQ("EQ"),
    NEQ("NEQ"),
    GREATEREQ("GREATEREQ"),
    SMALLEREQ("SMALLEREQ"),
    SMALLER("SMALLER"),
    GREATER("GREATER"),
    PLUS("PLUS"),
    MINUS("MINUS"),
    // dtypes
    DTSTRING("DTSTRING"),
    DTINT("DTINT"),
    DTBOOLEAN("DTBOOLEAN"),
    NOT("NOT"),
    // boolop
    AND("AND"),
    OR("OR"),
    DIV("DIV"),
    IMPLIES("IMPLIES");

    private String name;

    LanguageElements(String name){
        this.name = name;
    }
}
