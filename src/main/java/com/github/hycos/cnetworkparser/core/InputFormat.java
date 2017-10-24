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

package com.github.hycos.cnetworkparser.core;


import com.github.hycos.cnetworkparser.lang.smt.Z3Str2Listener;
import com.github.hycos.cnetworkparser.lang.smt.CVC4Listener;
import com.github.hycos.cnetworkparser.lang.smt.S3Listener;
import com.github.hycos.cnetworkparser.lang.sol.SolListener;

public enum InputFormat {

    SOL(0, "Sol","Sol.g4"),
    CVC4 (1, "CVC4", "CVC4.g4"),
    Z3STR2(2, "Z3Str2", "Z3Str2.g4"),
    LOGIC(3, "Logic", "Logic.g4"),
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
            case "logic": return LOGIC;
        }
        return null;
    }


    public ConstraintNetworkProvider getProvider() {
        switch (this) {
            case SOL: return new SolListener();
            case CVC4: return new CVC4Listener();
            case Z3STR2: return new Z3Str2Listener();
            case S3: return new S3Listener();
        }
        return null;
    }

    public static String getAvailableFormats() {
        return "(" + SOL.getName() + "," + CVC4.getName() + "," + Z3STR2.getName() + "," + S3.getName() +")";
    }



}
