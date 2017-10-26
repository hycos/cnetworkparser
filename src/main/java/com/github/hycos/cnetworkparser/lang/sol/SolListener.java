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

package com.github.hycos.cnetworkparser.lang.sol;

import com.github.hycos.cnetwork.api.NodeKindFactoryInterface;
import com.github.hycos.cnetwork.api.NodeKindInterface;
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.*;
import com.github.hycos.cnetworkparser.core.ConstraintNetworkBuilderFactoryInterface;
import com.github.hycos.cnetworkparser.core.ConstraintNetworkGenerator;
import com.github.hycos.cnetworkparser.exception.ParserRuntimeException;
import com.github.hycos.cnetworkparser.threatmodels.ThreatModel;
import com.github.hycos.cnetworkparser.threatmodels.ThreatModelFactory;
import com.github.hycos.cnetworkparser.utils.StringPair;
import com.github.hycos.cnetworkparser.utils.StringUtils;
import dk.brics.automaton.Automaton;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.listener.DefaultListener;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

// TODO: refactor parser: proper post-order traversal
public class SolListener extends DefaultListener implements ConstraintNetworkGenerator {

    final static Logger LOGGER = LoggerFactory.getLogger(SolListener.class);

    private BasicConstraint constraint = new BasicConstraint();
    private ScopeMgr ctx = new ScopeMgr();

    private ConstraintNetworkBuilder cbuilder = null;
    private NodeKindFactoryInterface ni = null;
    private Map<String, ThreatModel> tmodel = null;

    public SolListener() {
        this.cbuilder = new ConstraintNetworkBuilder();
        this.ni = DefaultNodeKindFactory.INSTANCE;
        this.tmodel = ThreatModelFactory.INSTANCE.getAllThreatModels(this.ni);
    }

    public SolListener(ConstraintNetworkBuilderFactoryInterface bld) {
        this.cbuilder = bld.getConstraintNetworkBuilder();
        this.ni = bld.getNodeKindFactory();
        this.tmodel = ThreatModelFactory.INSTANCE.getAllThreatModels(this.ni);
    }

    public void addVariable(String type, String label) {
        Node op = null;

        //LOGGER.info("type " + type);
        //LOGGER.info("label " + label);

        if (type.equals("string")) {
            op = cbuilder.addOperand(ni.getNodeKindFromString("strvar"), label);
        } else if (type.equals("int")) {
            op = cbuilder.addOperand(ni.getNodeKindFromString("numvar"), label);
        } else if (type.equals("bool")) {
            op = cbuilder.addOperand(ni.getNodeKindFromString("boolvar"), label);
        }
        assert op != null;
    }

    public Node addConstraint(BasicConstraint con) throws
            InconsistencyException {
        Node c = this.cbuilder.addConstraint(con.opKind, con.nodes.toArray(new
                Node [con.nodes.size()]));

        LOGGER.debug(this.cbuilder.getConstraintNetwork().toDot());
        return c;
    }



    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() {
        return this.cbuilder;
    }

    @Override
    public ConstraintNetwork getConstraintNetwork() {
        return this.cbuilder.getConstraintNetwork();
    }

    @Override
    public DefaultListener getListener() {
        return this;
    }

    @Override
    public void visitTerminal(TerminalNode terminalNode) {
    }

    @Override
    public void visitErrorNode(ErrorNode errorNode) {
    }

    @Override
    public void enterEveryRule(ParserRuleContext ctx) {

        LOGGER.debug(">>> " + this.getRuleByKey(ctx.getRuleIndex()) + " " +
                ctx.getText());
        switch (this.getRuleByKey(ctx.getRuleIndex())) {
            case "s":
                this.ctx.push(this.getRuleByKey(ctx.getRuleIndex()), ctx.getText());
                break;

            case "searchop":
            case "constraint":
                constraint.clear();

            case "funccall":
            case "vardecl":
            case "funcdecl":
            case "assignment":
            case "link":
                this.ctx.enterNewCtx();

            case "string":
            case "number":
            case "boollit":
            case "vartype":
            case "videntifier":
            case "op":
            case "boolop":
            case "esc":
            case "comp":
            case "userdef":
            case "bytecodesig":
            case "tmodeltype":
            case "trans":
                this.ctx.push(this.getRuleByKey(ctx.getRuleIndex()), ctx.getText());
                break;

        }
    }


    @Override
    public void exitEveryRule(ParserRuleContext ctx) {


        //LOGGER.info("CONTEX >>> " +  this.getRuleByKey(ctx.getRuleIndex()));

        //LOGGER.info("+RECENT " + this.ctx.getRecentCtxId());
        switch (this.getRuleByKey(ctx.getRuleIndex())) {
            case "vardecl":
                handleVardecl();
                this.ctx.leaveOldCtx();
                break;
            case "funcdecl":
                handleFuncdecl();
                break;
            case "videntifier":
                //LOGGER.info("-- " + this.ctx.getRecentCtxId());
                if (!this.ctx.getRecentCtxId().equals("vardecl") &&
                        !this.ctx.getRecentCtxId().equals("funcdecl") &&
                        !this.ctx.getRecentCtxId().equals("link")) {
                    handleVidentifer(this.ctx.getRecentCxt().pop().value);
                }
                break;
            case "string":
                try {
                    handleString(this.ctx.getRecentCxt().pop().value);
                } catch (InconsistencyException e) {
                    LOGGER.error(e.getMessage());
                    System.exit(-1);
                }
                break;
            case "number":
                try {
                    handleNumber(this.ctx.getRecentCxt().pop().value);
                } catch (InconsistencyException e) {
                    LOGGER.error(e.getMessage());
                    System.exit(-1);
                }
                break;
            case "boollit":
                try {
                    handleBoollit(this.ctx.getRecentCxt().pop().value);
                } catch (InconsistencyException e) {
                    LOGGER.error(e.getMessage());
                    System.exit(-1);
                }
                break;
            case "funccall":
                try {
                    handleOp();
                } catch (InconsistencyException e) {
                    LOGGER.error(e.getMessage());
                    System.exit(-1);
                }
                this.ctx.leaveOldCtx();
                break;
            case "constraint":
                //this.ctx.leaveOldCtx();

                String rec = this.ctx.getRecentCxt().pop().value;

                LOGGER.debug("REC {}", rec);

                NodeKindInterface okind = ni.getNodeKindFromString(rec);

                LOGGER.debug("okind {}", okind.getDesc());

                constraint.setOpKind(okind);
                try {
                    handleConstraint();
                } catch (InconsistencyException e) {
                    throw new ParserRuntimeException(e.getMessage());
                    //e.printStackTrace();
                    // @TODO: change this lateron -- proper parser
                    // reimplementation for sol format required
                }
                this.ctx.leaveOldCtx();
                break;
            case "link":
                handleLink();
                this.ctx.leaveOldCtx();
                break;
        }
    }

    private void handleVidentifer(String name) {
        LOGGER.debug("get " + name);
        Node op = this.cbuilder.getNodeByLabel(name);
        assert (op != null);
        this.ctx.push(op);
    }


    private void handleLink()  {
        RuleScope cctx = this.ctx.getRecentCxt();
        StringPair rpoint = cctx.pop(); // videntifier
        StringPair label = cctx.pop(); // vatype
        Node n = null;


        LOGGER.info("Tmodel " + rpoint);
        LOGGER.info("Label " + label);

        if ((n = cbuilder.getNodeByLabel(label.value)) == null) {
            LOGGER.error("Parser Error: cannot link " + label.value + " to a threat model");
            System.exit(-1);
        }

        assert (!n.isNumeric());

        NodeKindInterface tmodeltype = ni.getNodeKindFromString(rpoint.value);

        //DefaultNodeKind tmodeltype = DefaultNodeKind.KindFromString(rpoint
        //        .value);

        ConstraintNetworkBuilder subnet = null;

        LOGGER.debug("XSS {}", tmodeltype);

        LOGGER.debug("keys {}", tmodel.keySet());
//        try {
            //subnet = ThreatModelFactory.getInstance().getCNforVulnerability
            //        (tmodeltype);
        assert tmodel.containsKey(tmodeltype.getValue());

            subnet = tmodel.get(tmodeltype.getValue()).delegate(tmodeltype);
//        } catch (UnknownException e) {
//            e.printStackTrace();
//        }

        assert subnet != null;

        try {
            cbuilder.join(ni.getNodeKindFromString("matches"), n, subnet);
        } catch (InconsistencyException e) {
            assert false;
        }
        //cn.addConstraint(NodeKind.MATCHES, n, subnet.getStartNode());

        // join threat model subnetwork with reference point
    }


    private void handleFuncdecl() {
        RuleScope cctx = this.ctx.getRecentCxt();
        StringPair id = cctx.pop(); // fidentifier
        StringPair bsig = cctx.pop(); // bsig
        this.cbuilder.registerExtOperation(StringUtils.trimQuotesFromString(bsig.value),
                StringUtils.trimQuotesFromString(id.value));

    }

    private void handleVardecl() {
        RuleScope cctx = this.ctx.getRecentCxt();
        StringPair id = cctx.pop(); // videntifier
        StringPair type = cctx.pop(); // vatype
        //LOGGER.info("ADD VAR " + id +  " " + type);
        addVariable(type.value, id.value);
    }

    private void handleString(String name) throws InconsistencyException {

        String fctx = this.ctx.getRecentCxt().peek().value;

        boolean rexp = false;

        LOGGER.debug("NAME before {}", name);

        String nname = StringUtils.trimQuotesFromString(name);
//        LOGGER.info("NNAME 1 " + nname);
        //nname = nname.replaceAll("\\\\\"", "\\\"");
//        LOGGER.info("NNAME 2 " + nname);
        //nname = StringUtils.trimQuotesFromString(nname);
//        LOGGER.info("NNAME 3 " + nname);

//        nname = EscapeUtils.escapeSpecialCharacters(nname);

        //String nname = name;

        LOGGER.info("FCTX " + fctx);
        if (!fctx.equals("tolit")) {
            //nname = EscapeUtils.unescapeSpecialCharacters(nname);
        } else {
            this.ctx.getRecentCxt().pop();
        }

//        LOGGER.info("NAME  " + name);
//        LOGGER.info("NNAME  " + nname);
        //Node string = this.cbuilder.getNodeByLabel(name);




        Node string = null;
       // if (string == null) {
            Automaton a = new dk.brics.automaton.RegExp(nname).toAutomaton();

            if (a.getSingleton() != null) {
                string = cbuilder.addOperand(ni.getNodeKindFromString("strlit"),nname);
            } else {
                string = cbuilder.addOperand(ni.getNodeKindFromString("strexp"),nname);
            }

//            NodeDomain nd = new NodeDomain(DomainKind.STRING, a, new NumRange(DomainUtils.getApproxLenRange(a)));
//
//            string.setDomain(nd);
        //}

        LOGGER.debug("push {} ---  {}", string.getDotLabel(),nname);

        this.ctx.push(string);

    }

    private void handleNumber(String name) throws InconsistencyException {
        LOGGER.debug("NUMBER " + name);
        Node number = cbuilder.addOperand(ni.getNodeKindFromString("numlit"), name);
        ctx.push(number);
    }

    private void handleBoollit(String name) throws InconsistencyException {
        Node boollit = cbuilder.addOperand(ni.getNodeKindFromString("boollit"), name);
        this.ctx.push(boollit);
    }

    public boolean areString(Collection<Node> nset) {
        for (Node n : nset) {
            if (n.isString())
                continue;

            return false;
        }
        return true;
    }

    public boolean areBool(Collection<Node> nset) {
        for (Node n : nset) {
            if (n.isBoolean())
                continue;

            return false;
        }
        return true;
    }


    public boolean areNum(Collection<Node> nset) {
        for (Node n : nset) {
            if (n.isNumeric())
                continue;

            return false;
        }
        return true;
    }

    private void handleOp() throws InconsistencyException {

        LinkedList<Node> params = this.ctx.getNodesForCtx();

        String sctx = this.ctx.getRecentCxt().getBackref().getName();

        //LOGGER.info("CALLING CTX " + sctx);

        StringPair op = this.ctx.pop(); //op

        LOGGER.debug("OPERATION " + op);

        String type = op.key; // funccall
        String skind = op.value;

        //LOGGER.info("TYPE " + type);
        //LOGGER.info("Kind " + skind);

        String fcall = this.ctx.pop().value; //

        //String fcall = id;

        LOGGER.debug(" BOOO FCALL " + fcall);
        //LOGGER.info("OP " + op);


        //LOGGER.info("PARAMS " + params.size());

        LOGGER.debug("skind {}", skind);
        NodeKindInterface okind = ni.getNodeKindFromString(skind);

        LOGGER.info("OKIND " + okind);
        Node newop = null;

        // infer the types for overloaded operators (==)

        if (okind == ni.getNodeKindFromString("==")) {
            /**if (areString(params))
                okind = DefaultNodeKind.STR_EQUALS;
            else if (areNum(params))
                okind = DefaultNodeKind.NUM_EQUALS;
            else if (areBool(params))
                okind = DefaultNodeKind.BOOL_EQUALS;**/
            okind = ni.getNodeKindFromString("==");
        } else if (okind == ni.getNodeKindFromString("!=")) {
//            if (areString(params))
//                okind = DefaultNodeKind.STR_NEQUALS;
//            else if (areNum(params))
//                okind = DefaultNodeKind.NUM_NEQUALS;
//            else if (areBool(params))
//                okind = DefaultNodeKind.BOOL_NEQUALS;
            okind = ni.getNodeKindFromString("==");
        }

        if (okind == ni.getNodeKindFromString("unknown")) {
            newop = this.cbuilder.addExtOperation(skind, params);
        } else {
            newop = this.cbuilder.addOperation(okind, params);
        }

        LOGGER.info("NEWOP KIND " + newop.getKind().toString());

        if (newop.getKind() == ni.getNodeKindFromString("ite")) {

            assert params.size() == 3;

            assert (params.get(1).isNumeric() && params.get(2).isNumeric()) ||
                    (params.get(1).isString() && params.get(2).isString()) ||
                    (params.get(1).isBoolean() && params.get(2).isBoolean());

            LOGGER.debug("1 ite " + params.get(1).getKind() + " " + params.get
                    (2).getKind());

            Operation parop = null;

            if (params.get(1).isOperation())
                parop = (Operation) params.get(1);
            else if (params.get(2).isOperation())
                parop = (Operation) params.get(2);

            // @Julian: change lateron
            /**if (parop != null) {
                alterDomain(newop, params.get(1).getKind
                        ().getDomainKind());
            } else {
                if (params.get(1).isBoolean()) {
                    alterDomain(newop, DomainKind.BOOLEAN);
                } else if (params.get(1).isString()) {
                    alterDomain(newop, DomainKind.STRING);
                }
            }**/
        }


        // safe node (context can be either term or funccall
        this.ctx.getRecentCxt().getBackref().addNode(newop);
    }

    /**private void alterDomain(Node n, DomainKind newKind) {
        NodeKind kind = n.getKind();
        try {
            kind.setDomainKind(newKind);
        } catch (IllegalDomainException e) {
            LOGGER.error(e.getMessage());
            System.exit(-1);
        }
        NodeDomain dkind = NodeDomainFactory.INSTANCE.getDomainForKind
                (kind);
        dkind.setDomain(dkind);
    }**/

    private Node handleConstraint() throws InconsistencyException {
        //LOGGER.debug("HANDLE ctx" + this.ctx.getRecentCxt().getName());
        List<Node> nods = this.ctx.getNodesForCtx();
        //LOGGER.info("SS " + nods.size());
        //assert(nods.size() == 2);

        LOGGER.debug("opKind {}", constraint.opKind.getDesc());
        if (nods.size() == 2) {
            constraint.addNode(nods.get(0));
            constraint.addNode(nods.get(1));
//
//            boolean negate = (constraint.opKind == NodeKind.NEQUALS);
//
//            LOGGER.info("OPKIND " + constraint.opKind.getDesc());
//            // type inference
//            if (constraint.opKind == NodeKind.EQUALS || negate) {
//                if (constraint.isNumeric()) {
//                    constraint.setOpKind(NodeKind.NUM_EQUALS);
//                } else if (constraint.isString()) {
//                    constraint.setOpKind(NodeKind.STR_EQUALS);
//                } else if (constraint.isBoolean()) {
//                    constraint.setOpKind(NodeKind.BOOL_EQUALS);
//                } else {
//                    assert (false);
//                }
//            }
//            assert constraint.opKind != null;

            LOGGER.debug("CONSTRAINT {}", constraint.toString());

            Node c = addConstraint(constraint);
//
//            if (negate) {
//                c.setDomain(NodeDomainFactory.DBFALSE.clone());
//            }

            return c;

        } else if (nods.size() == 1) {
            switch (constraint.opKind.getValue().toUpperCase()) {
                case "EMPTY":
                    return this.cbuilder.addConstraint(ni.getNodeKindFromString("empty"), nods
                            .get(0));
                case "NOT":
                    return this.cbuilder.addConstraint(ni.getNodeKindFromString("not"), nods.get(0));
            }
        }

        LOGGER.info("KKK 1" + constraint.toString());

        assert false;
        return null;
    }

}