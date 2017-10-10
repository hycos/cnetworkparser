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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.domain.NodeDomainFactory;
import com.github.hycos.cnetwork.core.graph.*;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;
import com.github.hycos.cnetworkparser.utils.QuadrupleMap;
import com.github.hycos.cnetworkparser.utils.StringUtils;
import org.snt.inmemantlr.exceptions.ParseTreeProcessorException;
import org.snt.inmemantlr.tree.ParseTree;
import org.snt.inmemantlr.tree.ParseTreeNode;
import org.snt.inmemantlr.tree.ParseTreeProcessor;

import java.util.List;
import java.util.Set;
import java.util.Vector;

public class SmtCnetworkBuilder extends
        ParseTreeProcessor<ConstraintNetworkBuilder, Node> {


    final static Logger LOGGER = LoggerFactory.getLogger(SmtCnetworkBuilder.class);

    // data struture that contains the mapping between language elements of
    // source language to Constraint network constructs
    public static class TransMap extends QuadrupleMap<LanguageElements,
            String,String,NodeKind>{

        public String getNameOfElement(LanguageElements e) {
            return this.getSecondByFirst(e);
        }

        public String getTranslationByElement(LanguageElements e) {
            return this.getThirdByFirst(e);
        }

        public NodeKind getNodeKindByLabel(String label) {
            return this.getFourthBySecond(label);
        }

    };


    protected static TransMap TRANSMAP;

    protected ConstraintNetworkBuilder cn = null;

    public SmtCnetworkBuilder(ParseTree parseTree, final TransMap tm) {
        super(parseTree);
        cn = new ConstraintNetworkBuilder();
        TRANSMAP = tm;
    }


    /**
     * Preprocesing of Regular expressions (if present)
     */
    private void preprocessRegex() {
        for(ParseTree subtree : this.parseTree.getSubtrees(n -> n.getRule().equals("regexoperation"))) {
            SmtRegexBuilder rbuilder = new SmtRegexBuilder(subtree,TRANSMAP);
        }
    }

    @Override
    public ConstraintNetworkBuilder process() throws ParseTreeProcessorException {

        /**
         * Get regular expression subtrees and translate them
         * into the standard form. Replace the subtree roots
         * with the tranlation.
         */
        Set<ParseTree> regex = parseTree.getDominatingSubtrees(n -> n.getRule().equals("regexoperation"));
        for(ParseTree r : regex) {
            //LOGGER.info(r.toDot());
            SmtRegexBuilder regexbuilder = new SmtRegexBuilder(r,TRANSMAP);
            String sregex = null;
            sregex = regexbuilder.process();

            //LOGGER.info("REGEX " + sregex);
            // print changed tree
            ParseTree replacement = new ParseTree("rlit", sregex);
            parseTree.replaceSubtree(r, replacement);
        }

        /**
         * get variable declarations
         */
        Set<ParseTree> vdef = parseTree.getDominatingSubtrees(n -> n.getRule().matches("(funcdecl|vardecl)"));
        for(ParseTree r : vdef) {
            assert(r.getRoot().getChildren().size() == 2);

            ParseTreeNode varname = r.getRoot().getFirstChild();
            ParseTreeNode vartype = r.getRoot().getLastChild();
            NodeKind kind = NodeKind.UNKNOWN;

            //LOGGER.info("KIND " + kind.toString());
            switch (vartype.getLabel().toLowerCase()) {
                case "string":
                    kind = NodeKind.STRVAR;
                    break;
                case "bool":
                    kind = NodeKind.BOOLVAR;
                    break;
                case "int":
                    kind = NodeKind.NUMVAR;
                    break;
            }



            //this.cn.addNode(op);
            cn.addOperand(kind, varname.getLabel());
            parseTree.removeSubtree(r);
        }

        //LOGGER.info(this.parseTree.toDot());
        // process the remaining tree and build constraint network

        ConstraintNetworkBuilder cn = super.process();

        //LOGGER.info(debug());
        return cn;
    }


    public ConstraintNetworkBuilder getResult() {
        return this.cn;
    }

    @Override
    protected void initialize() {

        //LOGGER.info(parseTree.toDot());
        for(ParseTreeNode n : this.parseTree.getNodes()) {
            this.smap.put(n, null);
        }
    }

    @Override
    protected void process(ParseTreeNode n) throws ParseTreeProcessorException {

        //LOGGER.info("ID " + n.getId() + " " + n.getRule() + " " + n.getLabel
        //        ());

        try {
            switch (n.getRule()) {

                case "booloperation":
                case "stroperation":
                case "numoperation":
                case "binoperation":

                    //LOGGER.info("OP " + n.getId());
                    assert (n.hasChildren());
                    StringBuilder out = new StringBuilder();

                    ParseTreeNode fchild = n.getFirstChild();
                    assert (fchild.getRule().matches("(boolop|strop|numop|binop)"));

                    List<Node> params = new Vector<Node>();

                    //LOGGER.info("LBL " + fchild.getLabel());

                    NodeKind kind = TRANSMAP.getNodeKindByLabel(fchild.getLabel());

                    assert (kind != null);
                    assert ((kind instanceof NodeKind));

                    //LOGGER.info("FHILD " + fchild.getLabel());

                    n.getChildren().stream().filter(c -> !c.equals(fchild)).forEach(
                            e -> {
                                //assert (this.smap.containsKey(e));
                                //LOGGER.info("get " + e.getLabel());
                                //assert (this.smap.get(e) != null);
                                params.add(this.smap.get(e));
                            }
                    );
                    Node op = null;


                    if (n.getRule().equals("numoperation") && params.size() == 1) {
                        // unary operation
                        assert (fchild.getLabel().matches("(\\-|\\+)"));
                        String no = fchild.getLabel();
                        for (Node p : params) {
                            no += p.getLabel();
                        }
                        //op = this.cn.addNode(new Operand(no, NodeKind
                        // .NUMLIT));

                        op = cn.addOperand(NodeKind.NUMLIT, no);
                        //LOGGER.info("add transformed node " + op.getLabel());
                    } else {
                        // sometimes the indexof operator assumes a startin index
                        // of 0 implicitly
                        //if (kind == NodeKind.INDEXOF && params.size() == 2) {
                        //    params.add(new Operand("0", NodeKind.NUMLIT));
                        //}

                        op = this.cn.addOperation(kind, params);
                        //LOGGER.info("add Operation " + op.getLabel());
                    }

                    this.smap.put(n, op);
                    break;
                case "boollit":
                    //Node blit = this.cn.addNode(new Operand(n.getLabel(),
                    //    NodeKind.BOOLLIT));
                    Node blit = cn.addOperand(NodeKind.BOOLLIT, n.getLabel());
                    this.smap.put(n, blit);
                    break;
                case "varname":
                    //LOGGER.debug(cn.getConstraintNetwork().toDot());
                    //LOGGER.debug(cn.getEufLattice().toDot());
                    Node v = cn.getNodeByLabel(n.getLabel());
                    assert (v != null);
                    this.smap.put(n, v);
                    break;
                case "rlit":
                    //LOGGER.info("rlit " + n.getLabel());
                    //Node r = this.cn.addNode(new Operand(n.getLabel(),
                    //    NodeKind.STRREXP));

                    Node r = cn.addOperand(NodeKind.STRREXP, n.getLabel());
                    this.smap.put(n, r);
                    break;
                case "strlit":
                    assert (n.getLabel().length() >= 2);
                    String lbl = n.getLabel().substring(1, n.getLabel().length() - 1);
                    lbl = StringUtils.unescapeSpecialCharacters(lbl);
                    LOGGER.debug("add strlit {}", lbl);
                    Node nn = cn.addOperand(NodeKind.STRLIT, lbl);
                    this.smap.put(n, nn);
                    break;
                case "number":
                    //LOGGER.info("nunber " + n.getLabel());
                    //Node nlit = this.cn.addNode(new Operand(n.getLabel(),
                    //    NodeKind.NUMLIT));

                    Node nl = cn.addOperand(NodeKind.NUMLIT, n.getLabel());
                    this.smap.put(n, nl);
                    break;
                case "operation":
                case "param":
                case "s":
                    simpleProp(n);
                    break;
                case "assertion":
                    Node c = this.smap.get(n.getFirstChild());
                    // ite is a special case ... the return value remains
                    // parametrized
                    //if(c.getKind() != NodeKind.ITE) {
                    c.setDomain(NodeDomainFactory.DBTRUE);

                    assert ((c instanceof Operation) || (c instanceof Operand));
                    /**Operand top = new Operand("true", NodeKind.BOOLLIT);

                    // if then else is an exception the outcome of ITE is
                    // determined by the predicate
                    LOGGER.debug("ASSERTION " + c.getLabel());

                    Node constraint = null;
                    try {
                        constraint = cn.addConstraint(NodeKind.BOOL_EQUALS, top, c);
                    } catch (EUFInconsistencyException e) {
                        throw new ParseTreeProcessorException(e.getMessage());
                    }**/
                    this.smap.put(n, c);
                    //} else {
                    //    simpleProp(n);
                    //}
                    break;
                case "copoperation":
                    NodeKind copkind = TRANSMAP
                            .getNodeKindByLabel(n.getFirstChild().getLabel());

                    if (copkind == NodeKind.ITE) {

                        assert n.getChildren().size() == 4;

                        Node par1 = this.smap.get(n.getChild(1));
                        Node par2 = this.smap.get(n.getChild(2));
                        Node par3 = this.smap.get(n.getChild(3));

                        assert par2.isBoolean() && par3.isBoolean();
                        LOGGER.debug("par " + par1.getLabel());
                        LOGGER.debug("knd " + par1.getKind());
                        assert par1.isBoolean();


                        //par1.setDomain(NodeDomainFactory.DBTRUE);
                        //par2.setDomain(NodeDomainFactory.DBTRUE);
                        //par3.setDomain(NodeDomainFactory.DBTRUE);


                        Node ite = cn.addOperation(NodeKind.ITE,
                                par1, par2, par3);

                        this.smap.put(n, ite);
                    }
            }
        } catch (EUFInconsistencyException e) {
            throw new ParseTreeProcessorException(e.getMessage());
        }
    }


}
