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


public class Z3Str2Listener extends DefaultTreeListener implements CnetworkProvider {

    public static final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder.TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "RegexStar", "*", OperationKind.UNKNOWN);
        put(LanguageElements.KPLUS, "RegexPlus", "+", OperationKind.UNKNOWN);
        put(LanguageElements.UNION, "RegexUnion", "|", OperationKind.UNKNOWN);
        put(LanguageElements.RCONCAT, "RegexConcat", "", OperationKind.CONCAT);
        put(LanguageElements.OPT, "", "", OperationKind.UNKNOWN); // not available
        put(LanguageElements.RAN, "RegexCharRange", "", OperandKind.UNKNOWN);
        put(LanguageElements.CONV, "Str2Reg", "", OperationKind.UNKNOWN); // not available
        put(LanguageElements.MATCHES, "RegexIn", OperationKind.MATCHES.getValue(), OperationKind.MATCHES);
        // for string operations
        put(LanguageElements.LEN, "Length", OperationKind.LEN.getValue(), OperationKind.LEN);
        put(LanguageElements.CONTAINS, "Contains", OperationKind.CONTAINS.getValue(), OperationKind.CONTAINS);
        put(LanguageElements.INDEXOF, "IndexOf", OperationKind.INDEXOF.getValue(), OperationKind.INDEXOF);
        put(LanguageElements.INDEXOF, "IndexOf2", OperationKind.INDEXOF.getValue(), OperationKind.INDEXOF);
        put(LanguageElements.INDEXOF, "LastIndexOf", OperationKind.INDEXOF.getValue(), OperationKind.INDEXOF);
        put(LanguageElements.PREFIXOF, "StartsWith", OperationKind.STARTSWITH.getValue(), OperationKind.STARTSWITH);
        put(LanguageElements.SUFFIXOF, "EndsWith", OperationKind.ENDSWITH.getValue(), OperationKind.ENDSWITH);
        put(LanguageElements.SCONCAT, "Concat", OperationKind.CONCAT.getValue(), OperationKind.CONCAT);
        put(LanguageElements.SUBSTRNG, "Substring", OperationKind.SUBSTR.getValue(), OperationKind.SUBSTR);
        // others
        put(LanguageElements.NEQ, "!=", OperationKind.NEQUALS.toString(), OperationKind.NEQUALS);
        put(LanguageElements.EQ, "=", OperationKind.EQUALS.toString(), OperationKind.EQUALS);
        put(LanguageElements.SMALLEREQ, "<=", OperationKind.SMALLEREQ.toString(), OperationKind.SMALLEREQ);
        put(LanguageElements.GREATEREQ, ">=", OperationKind.GREATEREQ.toString(), OperationKind.GREATEREQ);
        put(LanguageElements.SMALLER, "<", OperationKind.SMALLER.toString(), OperationKind.SMALLER);
        put(LanguageElements.GREATER, ">", OperationKind.GREATER.toString(), OperationKind.GREATER);
        put(LanguageElements.PLUS, "+", OperationKind.ADD.toString(), OperationKind.ADD);
        put(LanguageElements.MINUS, "-", OperationKind.SUB.toString(), OperationKind.SUB);
        //variables
        put(LanguageElements.DTSTRING, "String", OperandKind.STRVAR.toString(), OperandKind.STRVAR);
        put(LanguageElements.DTINT, "Int", OperandKind.NUMVAR.toString(), OperandKind.NUMVAR);
        put(LanguageElements.DTBOOLEAN, "Bool", OperandKind.BOOLVAR.toString(), OperandKind.BOOLVAR);
        // boolean operations
        put(LanguageElements.AND, "and", OperationKind.AND.toString(), OperationKind.AND);
        put(LanguageElements.OR, "or", OperationKind.OR.toString(), OperationKind.OR);
        put(LanguageElements.NOT, "not", OperationKind.NOT.toString(), OperationKind.NOT);
        // if-then-else
        put(LanguageElements.ITE, "ite", OperationKind.ITE.toString(), OperationKind.ITE);
        // division
        put(LanguageElements.DIV, "div", OperationKind.DIV.toString(), OperationKind.DIV);
    }};

    final static Logger LOGGER = LoggerFactory.getLogger(Z3Str2Listener.class);

    public Z3Str2Listener() {
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
