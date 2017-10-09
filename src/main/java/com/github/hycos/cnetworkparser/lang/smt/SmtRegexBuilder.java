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
