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
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIESni.getNodeKindFromString("or")
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetworkparser.lang.smt;

import com.github.hycos.cnetworkparser.core.ConstraintNetworkBuilderFactoryInterface;
import com.github.hycos.cnetworkparser.core.ConstraintNetworkCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.listener.DefaultListener;


public class CVC4Listener extends ConstraintNetworkCreator {

    final static Logger LOGGER = LoggerFactory.getLogger(CVC4Listener.class);

    public final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder.TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "re.*", "*", ni.getNodeKindFromString
                ("unknown"));
        put(LanguageElements.KPLUS, "re.+", "+", ni.getNodeKindFromString
                ("unknown"));
        put(LanguageElements.UNION, "re.union", "|", ni.getNodeKindFromString
                ("unknown"));
        put(LanguageElements.RCONCAT, "re.++", "", ni.getNodeKindFromString
                ("concat"));
        put(LanguageElements.OPT, "re.opt", "?", ni.getNodeKindFromString
                ("unknown"));
        put(LanguageElements.RAN, "re.range", "", ni.getNodeKindFromString
                ("unknown"));
        put(LanguageElements.CONV, "str.to.re", "", ni.getNodeKindFromString
                ("unknown"));
        put(LanguageElements.MATCHES, "str.in.re", ni.getNodeKindFromString("matches").getValue(), ni.getNodeKindFromString("matches"));
        // for string operations
        put(LanguageElements.LEN, "str.len", ni.getNodeKindFromString
                        ("len").getValue(), ni.getNodeKindFromString("len"));
        put(LanguageElements.CONTAINS, "str.contains", ni.getNodeKindFromString
                ("contains").getValue(), ni.getNodeKindFromString
                ("contains"));
        put(LanguageElements.INDEXOF, "str.indexof", ni.getNodeKindFromString
                ("indexof").getValue(), ni.getNodeKindFromString
                ("indexof"));
        put(LanguageElements.PREFIXOF, "str.prefixof", ni.getNodeKindFromString
                ("startswith").getValue(), ni.getNodeKindFromString
                ("startswith"));
        put(LanguageElements.SUFFIXOF, "str.suffixof", ni.getNodeKindFromString
                ("endswith").getValue(), ni.getNodeKindFromString
                ("endswith"));
        put(LanguageElements.SCONCAT, "str.++", ni.getNodeKindFromString("concat").getValue(), ni.getNodeKindFromString("concat"));
        put(LanguageElements.SUBSTRNG, "str.substr", ni.getNodeKindFromString("substr").getValue(), ni.getNodeKindFromString("substr"));
        // others
        put(LanguageElements.NEQ, "!=", ni.getNodeKindFromString("!=").toString(), ni.getNodeKindFromString("!="));
        put(LanguageElements.EQ, "=", ni.getNodeKindFromString("==").toString(), ni.getNodeKindFromString("=="));
        put(LanguageElements.SMALLEREQ, "<=", ni.getNodeKindFromString("<=").toString(), ni.getNodeKindFromString("<="));
        put(LanguageElements.GREATEREQ, ">=", ni.getNodeKindFromString(">=").toString(), ni.getNodeKindFromString(">="));
        put(LanguageElements.SMALLER, "<", ni.getNodeKindFromString("<").toString(), ni.getNodeKindFromString("<"));
        put(LanguageElements.GREATER, ">", ni.getNodeKindFromString(">").toString(), ni.getNodeKindFromString(">"));
        put(LanguageElements.PLUS, "+", ni.getNodeKindFromString("add")
                .toString(), ni.getNodeKindFromString("add"));
        put(LanguageElements.MINUS, "-", ni.getNodeKindFromString("sub")
                .toString(), ni.getNodeKindFromString("sub"));
        //v ariables
        put(LanguageElements.DTSTRING, "String", ni.getNodeKindFromString("strvar").toString(), ni.getNodeKindFromString("strvar"));
        put(LanguageElements.DTINT, "Int", ni.getNodeKindFromString("numvar").toString(), ni.getNodeKindFromString("numvar"));
        put(LanguageElements.DTBOOLEAN, "Bool", ni.getNodeKindFromString("boolvar").toString(), ni.getNodeKindFromString("boolvar"));
        // boolean operations
        put(LanguageElements.AND, "and", ni.getNodeKindFromString("and").toString(), ni.getNodeKindFromString("and"));
        put(LanguageElements.OR, "or", ni.getNodeKindFromString("or").toString(), ni.getNodeKindFromString("or"));
        put(LanguageElements.NOT, "not", ni.getNodeKindFromString("not").toString(), ni.getNodeKindFromString("not"));
    }};


    public CVC4Listener() {
        super();
    }

    public CVC4Listener(ConstraintNetworkBuilderFactoryInterface bld) {
        super(bld);
    }

    @Override
    public SmtCnetworkBuilder.TransMap getTransMap() {
        return tm;
    }


    @Override
    public DefaultListener getListener() {
        return this;
    }


}
