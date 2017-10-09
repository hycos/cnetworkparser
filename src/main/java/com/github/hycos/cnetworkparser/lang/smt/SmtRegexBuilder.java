package com.github.hycos.cnetworkparser.lang.smt;

import com.github.hycos.cnetworkparser.core.RegexParseTreeProcessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
;
import org.snt.inmemantlr.tree.ParseTree;
import org.snt.inmemantlr.tree.ParseTreeNode;
import org.snt.inmemantlr.utils.EscapeUtils;


public class SmtRegexBuilder extends RegexParseTreeProcessor {

    final static Logger LOGGER = LoggerFactory.getLogger(SmtRegexBuilder.class);
    static SmtCnetworkBuilder.TransMap TRANSMAP;

    public SmtRegexBuilder(ParseTree ast, final SmtCnetworkBuilder.TransMap tm) {
        super(ast);
        this.TRANSMAP = tm;
    }

    private boolean is(ParseTreeNode n, LanguageElements e){
        return n.getLabel().equals(TRANSMAP.getNameOfElement(e));
    }


    public String getResult() {
        return this.smap.get(this.parseTree.getRoot());
    }


    @Override
    protected void process(ParseTreeNode n) {

        //LOGGER.info(n.getRule() + " " + n.getLabel());
        switch(n.getRule()){
            case "assert":
                simpleProp(n);
                break;
            case "s":
                simpleProp(n);
                break;
            case "regexop":
                if(n.isLeaf()) {
                    smap.put(n, TRANSMAP.getThirdBySecond(n.getLabel()));
                }
                break;
            case "regexoperation": {
                assert(n.hasChildren());
                StringBuilder out = new StringBuilder();


                ParseTreeNode fchild = n.getFirstChild();
                if (is(fchild, LanguageElements.UNION)) {
                    out.append("(");
                    n.getChildren().stream().filter(c -> !c.equals(fchild)).forEach(
                            e -> {
                                out.append(getElement(e) +
                                        (!n.getLastChild().equals(e) ? getElement(fchild): ""));
                            }
                    );
                    out.append(")");
                } else if(is(fchild, LanguageElements.RAN)) {
                    assert(n.getChildren().size() == 3);
                    out.append("[");
                    out.append(EscapeUtils.escapeSpecialCharacters(getElement(n.getChild(1))));
                    out.append("-");
                    out.append(EscapeUtils.escapeSpecialCharacters(getElement(n.getChild(2))));
                    out.append("]");
                } else {
                    n.getChildren().stream().filter(c -> !c.equals(n.getFirstChild())).forEach(
                            e -> out.append(getElement(e))
                    );
                    out.append(getElement(n.getFirstChild()));
                }

                this.smap.put(n, out.toString());
            }
            break;
            case "param":
            case "operation":
                simpleProp(n);
                break;
            case "strlit":
                assert(n.getLabel().length() > 2);
                String lbl = n.getLabel().substring(1,n.getLabel().length()-1);
                this.smap.put(n, EscapeUtils.escapeSpecialCharacters(lbl));
                break;
            case "allchar":
                this.smap.put(n, ".");
                break;
            case "parlist": {
                StringBuilder out = new StringBuilder();
                n.getChildren().forEach(
                        c -> out.append(this.smap.get(c))
                );
                this.smap.put(n, out.toString());
            }
            break;
        }
    }

}
