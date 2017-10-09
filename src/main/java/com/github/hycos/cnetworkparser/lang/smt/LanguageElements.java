/*
 * cnetworkparser - generate constraint network from different formats
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetworkparser is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetworkparser.lang.smt;

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
    LASTINDEXOF("LASTINDEXOF"),
    PREFIXOF("PREFIXOF"),
    SUFFIXOF("SUFFIXOF"),
    SCONCAT("SCONCAT"),
    SUBSTRNG("SUBSTRING"),
    REPLACE("REPLACE"),
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
