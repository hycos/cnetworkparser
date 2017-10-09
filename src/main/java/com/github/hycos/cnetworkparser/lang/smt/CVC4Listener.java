package com.github.hycos.cnetworkparser.lang.smt;

import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;
import com.github.hycos.cnetworkparser.core.ConstraintNetworkCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.exceptions.ParseTreeProcessorException;
import org.snt.inmemantlr.listener.DefaultListener;
import org.snt.inmemantlr.tree.ParseTree;

import static com.github.hycos.cnetwork.core.graph.NodeKind.*;


public class CVC4Listener extends ConstraintNetworkCreator {

    final static Logger LOGGER = LoggerFactory.getLogger(CVC4Listener.class);

    public static final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder.TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "re.*", "*", UNKNOWN);
        put(LanguageElements.KPLUS, "re.+", "+", UNKNOWN);
        put(LanguageElements.UNION, "re.union", "|", UNKNOWN);
        put(LanguageElements.RCONCAT, "re.++", "", CONCAT);
        put(LanguageElements.OPT, "re.opt", "?", UNKNOWN);
        put(LanguageElements.RAN, "re.range", "", UNKNOWN);
        put(LanguageElements.CONV, "str.to.re", "", UNKNOWN);
        put(LanguageElements.MATCHES, "str.in.re", MATCHES.getValue(), MATCHES);
        // for string operations
        put(LanguageElements.LEN, "str.len", LEN.getValue(), LEN);
        put(LanguageElements.CONTAINS, "str.contains", CONTAINS.getValue(), CONTAINS);
        put(LanguageElements.INDEXOF, "str.indexof", INDEXOF.getValue(), INDEXOF);
        put(LanguageElements.PREFIXOF, "str.prefixof", STARTSWITH.getValue(), STARTSWITH);
        put(LanguageElements.SUFFIXOF, "str.suffixof", ENDSWITH.getValue(), ENDSWITH);
        put(LanguageElements.SCONCAT, "str.++", CONCAT.getValue(), CONCAT);
        put(LanguageElements.SUBSTRNG, "str.substr", SUBSTR.getValue(), SUBSTR);
        // others
        put(LanguageElements.NEQ, "!=", NEQUALS.toString(), NEQUALS);
        put(LanguageElements.EQ, "=", EQUALS.toString(), EQUALS);
        put(LanguageElements.SMALLEREQ, "<=", SMALLEREQ.toString(), SMALLEREQ);
        put(LanguageElements.GREATEREQ, ">=", GREATEREQ.toString(), GREATEREQ);
        put(LanguageElements.SMALLER, "<", SMALLER.toString(), SMALLER);
        put(LanguageElements.GREATER, ">", GREATER.toString(), GREATER);
        put(LanguageElements.PLUS, "+", ADD.toString(), ADD);
        put(LanguageElements.MINUS, "-", SUB.toString(), SUB);
        //v ariables
        put(LanguageElements.DTSTRING, "String", STRVAR.toString(), STRVAR);
        put(LanguageElements.DTINT, "Int", NUMVAR.toString(), NUMVAR);
        put(LanguageElements.DTBOOLEAN, "Bool", BOOLVAR.toString(), BOOLVAR);
        // boolean operations
        put(LanguageElements.AND, "and", AND.toString(), AND);
        put(LanguageElements.OR, "or", OR.toString(), OR);
        put(LanguageElements.NOT, "not", NOT.toString(), NOT);
    }};


    public CVC4Listener() {
        super();
    }

    @Override
    public DefaultListener getListener() {
        return this;
    }


    @Override
    public ConstraintNetwork getConstraintNetwork() throws EUFInconsistencyException {
        ParseTree ast = this.getParseTree();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,tm);
        try {
            return builder.process().getConstraintNetwork();
        } catch (ParseTreeProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }

    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() throws EUFInconsistencyException {
        ParseTree ast = this.getParseTree();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,tm);
        try {
            return builder.process();
        } catch (ParseTreeProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }
}
