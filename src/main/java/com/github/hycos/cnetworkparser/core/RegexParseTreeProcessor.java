package com.github.hycos.cnetworkparser.core;

import org.snt.inmemantlr.tree.ParseTree;
import org.snt.inmemantlr.tree.ParseTreeNode;
import org.snt.inmemantlr.tree.ParseTreeProcessor;


public abstract class RegexParseTreeProcessor
        extends ParseTreeProcessor<String,String> {

    public RegexParseTreeProcessor(ParseTree ast) {
        super(ast);
    }

    @Override
    protected void initialize() {
        for(ParseTreeNode n : this.parseTree.getNodes()) {
            this.smap.put(n, "");
        }
    }

}
