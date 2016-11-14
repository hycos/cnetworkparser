package org.snt.cnetworkparser.lang.smt;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.OperandKind;
import org.snt.cnetwork.core.OperationKind;
import org.snt.cnetworkparser.core.CnetworkProvider;
import org.snt.inmemantlr.DefaultListener;
import org.snt.inmemantlr.DefaultTreeListener;
import org.snt.inmemantlr.tree.Ast;


public class CVC4Listener extends DefaultTreeListener implements CnetworkProvider {

    final static Logger LOGGER = LoggerFactory.getLogger(CVC4Listener.class);

    public static final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder.TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "re.*", "*", OperationKind.UNKNOWN);
        put(LanguageElements.KPLUS, "re.+", "+", OperationKind.UNKNOWN);
        put(LanguageElements.UNION, "re.union", "|", OperationKind.UNKNOWN);
        put(LanguageElements.RCONCAT, "re.++", "", OperationKind.CONCAT);
        put(LanguageElements.OPT, "re.opt", "?", OperationKind.UNKNOWN);
        put(LanguageElements.RAN, "re.range", "", OperandKind.UNKNOWN);
        put(LanguageElements.CONV, "str.to.re", "", OperationKind.UNKNOWN);
        put(LanguageElements.MATCHES, "str.in.re", OperationKind.MATCHES.getValue(), OperationKind.MATCHES);
        // for string operations
        put(LanguageElements.LEN, "str.len", OperationKind.LEN.getValue(), OperationKind.LEN);
        put(LanguageElements.CONTAINS, "str.contains", OperationKind.CONTAINS.getValue(), OperationKind.CONTAINS);
        put(LanguageElements.INDEXOF, "str.indexof", OperationKind.INDEXOF.getValue(), OperationKind.INDEXOF);
        put(LanguageElements.PREFIXOF, "str.prefixof", OperationKind.STARTSWITH.getValue(), OperationKind.STARTSWITH);
        put(LanguageElements.SUFFIXOF, "str.suffixof", OperationKind.ENDSWITH.getValue(), OperationKind.ENDSWITH);
        put(LanguageElements.SCONCAT, "str.++", OperationKind.CONCAT.getValue(), OperationKind.CONCAT);
        put(LanguageElements.SUBSTRNG, "str.substr", OperationKind.SUBSTR.getValue(), OperationKind.SUBSTR);
        // others
        put(LanguageElements.NEQ, "!=", OperationKind.NEQUALS.toString(), OperationKind.NEQUALS);
        put(LanguageElements.EQ, "=", OperationKind.EQUALS.toString(), OperationKind.EQUALS);
        put(LanguageElements.SMALLEREQ, "<=", OperationKind.SMALLEREQ.toString(), OperationKind.SMALLEREQ);
        put(LanguageElements.GREATEREQ, ">=", OperationKind.GREATEREQ.toString(), OperationKind.GREATEREQ);
        put(LanguageElements.SMALLER, "<", OperationKind.SMALLER.toString(), OperationKind.SMALLER);
        put(LanguageElements.GREATER, ">", OperationKind.GREATER.toString(), OperationKind.GREATER);
        put(LanguageElements.PLUS, "+", OperationKind.ADD.toString(), OperationKind.ADD);
        put(LanguageElements.MINUS, "-", OperationKind.SUB.toString(), OperationKind.SUB);
        //v ariables
        put(LanguageElements.DTSTRING, "String", OperandKind.STRVAR.toString(), OperandKind.STRVAR);
        put(LanguageElements.DTINT, "Int", OperandKind.NUMVAR.toString(), OperandKind.NUMVAR);
        put(LanguageElements.DTBOOLEAN, "Bool", OperandKind.BOOLVAR.toString(), OperandKind.BOOLVAR);
        // boolean operations
        put(LanguageElements.AND, "and", OperationKind.AND.toString(), OperationKind.AND);
        put(LanguageElements.OR, "or", OperationKind.OR.toString(), OperationKind.OR);
        put(LanguageElements.NOT, "not", OperationKind.NOT.toString(), OperationKind.NOT);
    }};


    public CVC4Listener() {
        super();
    }

    @Override
    public DefaultListener getListener() {
        return this;
    }

    @Override
    public ConstraintNetwork getConstraintNetwork() {

        Ast ast = this.getAst();

        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,tm);

        return builder.process();

    }
}
