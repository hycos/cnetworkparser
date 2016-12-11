package org.snt.cnetworkparser.lang.sol;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.core.domain.*;
import org.snt.cnetwork.exception.IllegalDomainException;
import org.snt.cnetwork.utils.DomainUtils;
import org.snt.cnetwork.utils.EscapeUtils;
import org.snt.cnetworkparser.core.CnetworkProvider;
import org.snt.cnetworkparser.exception.UnknownException;
import org.snt.cnetworkparser.threatmodels.ThreatModelFactory;
import org.snt.cnetworkparser.utils.StringUtils;
import org.snt.inmemantlr.listener.DefaultListener;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class SolListener extends DefaultListener implements CnetworkProvider {

    final static Logger LOGGER = LoggerFactory.getLogger(SolListener.class);

	private Map<String,Integer> rmap;

    private BasicConstraint constraint = new BasicConstraint();
    private ScopeMgr ctx = new ScopeMgr();

    private ConfigReader configReader = null;

	public SolListener() {
        this.configReader = new ConfigReader();
	}


    public ConfigReader getConfigReader() {
        return this.configReader;
    }


    @Override
    public DefaultListener getListener() {
        return this;
    }

    @Override
    public ConstraintNetwork getConstraintNetwork() {
        return this.configReader.getCnetwork();
    }


    @Override
    public void visitTerminal(TerminalNode terminalNode) {}

    @Override
    public void visitErrorNode(ErrorNode errorNode) {}

    @Override
	public void enterEveryRule(ParserRuleContext ctx) {

        LOGGER.info(">>> " + this.getRuleByKey(ctx.getRuleIndex()) + " " +
                ctx.getText());
		switch(this.getRuleByKey(ctx.getRuleIndex())) {
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
		switch(this.getRuleByKey(ctx.getRuleIndex())) {
            case "vardecl":
                handleVardecl();
                this.ctx.leaveOldCtx();
                break;
            case "funcdecl":
                handleFuncdecl();
                break;
            case "videntifier":
                //LOGGER.info("-- " + this.ctx.getRecentCtxId());
                if(!this.ctx.getRecentCtxId().equals("vardecl") &&
                        !this.ctx.getRecentCtxId().equals("funcdecl") &&
                        !this.ctx.getRecentCtxId().equals("link")) {
                    handleVidentifer(this.ctx.getRecentCxt().pop().value);
                }
                break;
            case "string":
                handleString(this.ctx.getRecentCxt().pop().value);
                break;
            case "number":
                handleNumber(this.ctx.getRecentCxt().pop().value);
                break;
            case "boollit":
                handleBoollit(this.ctx.getRecentCxt().pop().value);
                break;
            case "funccall":
                handleOp();
                this.ctx.leaveOldCtx();
                break;
            case "constraint":
                NodeKind okind = NodeKind.KindFromString(this.ctx.pop().value);
                constraint.setOpKind(okind);
                handleConstraint();
                this.ctx.leaveOldCtx();
                break;
            case "link":
                handleLink();
                this.ctx.leaveOldCtx();
                break;
		}
	}

    private void handleVidentifer(String name) {
        //LOGGER.info("get " + name);
        Operand op  = this.configReader.getOperandByLabel(name);
        assert(op != null);
        this.ctx.push(op);
    }


    private void handleLink() {
        RuleScope cctx = this.ctx.getRecentCxt();
        StringPair rpoint = cctx.pop(); // videntifier
        StringPair label = cctx.pop(); // vatype
        Node n = null;

        ConstraintNetwork cn = this.configReader.getCnetwork();

        //LOGGER.info("Tmodel " + rpoint);
        //LOGGER.info("Label " + label);

        if((n = cn.getNodeByLabel(label.value)) == null) {
            LOGGER.error("Parser Error: cannot link " + label.value + " to a threat model");
            System.exit(-1);
        }

        assert(!n.isNumeric());

        NodeKind tmodeltype = NodeKind.KindFromString(rpoint.value);

        ConstraintNetwork subnet = null;

        try {
            subnet = ThreatModelFactory.getInstance().getCNforVulnerability(tmodeltype);
        } catch (UnknownException e) {
            e.printStackTrace();
        }

        cn.join(NodeKind.MATCHES, n, subnet);
        //cn.addConstraint(NodeKind.MATCHES, n, subnet.getStartNode());

        // join threat model subnetwork with reference point
    }


    private void handleFuncdecl() {
        RuleScope cctx = this.ctx.getRecentCxt();
        StringPair id = cctx.pop(); // fidentifier
        StringPair bsig = cctx.pop(); // bsig
        this.configReader.getCnetwork().registerExtOperation(StringUtils.trimQuotesFromString(bsig.value),
                    StringUtils.trimQuotesFromString(id.value));

    }

    private void handleVardecl() {
        RuleScope cctx = this.ctx.getRecentCxt();
        StringPair id = cctx.pop(); // videntifier
        StringPair type = cctx.pop(); // vatype
        //LOGGER.info("ADD VAR " + id +  " " + type);
        this.configReader.addVariable(type.value, id.value);
    }

    private void handleString(String name) {

        String fctx = this.ctx.getRecentCxt().peek().value;

        boolean rexp = false;

        String nname = StringUtils.trimQuotesFromString(name);
        //LOGGER.info("NNAME 1 " + nname);
        nname = nname.replaceAll("\\\\\"","\\\"");
        //LOGGER.info("NNAME 2 " + nname);
        nname = StringUtils.trimQuotesFromString(nname);
        //LOGGER.info("NNAME 3 " + nname);

        //LOGGER.info("FCTX " + fctx);
        if(fctx.equals("tolit")) {
            this.ctx.getRecentCxt().pop();
            nname = EscapeUtils.escapeSpecialCharacters(nname);
        }

        //LOGGER.info("NAME  " + name);
        //LOGGER.info("NNAME  " + nname);
        Operand string = this.configReader.getOperandByLabel(name);


        if(string == null) {
            Automaton a = new Automaton(nname);

            if(a.getSingleton() != null) {
                string = new Operand(nname, NodeKind.STRLIT);
            } else {
                string = new Operand(nname, NodeKind.STRREXP);
            }

            NodeDomain nd = new NodeDomain(DomainKind.STRING,a,new NumRange(DomainUtils
                    .getApproxLenRange(a)));

            string.setDomain(nd);
            this.configReader.addNode(string);
        }

        this.ctx.push(string);

    }

    private void handleNumber(String name) {
        //LOGGER.info("NUMBER " + name);

        Operand number = this.configReader.getOperandByLabel(name);
        if (number == null) {
            number = new Operand(name, NodeKind.NUMLIT);
            configReader.addNode(number);
        }
        ctx.push(number);
    }

    private void handleBoollit(String name) {

        //LOGGER.info("DBOOL " + name);

        Operand boollit = this.configReader.getOperandByLabel(name);
        if (boollit == null) {
            boollit = new Operand(name, NodeKind.BOOLLIT);

            //LOGGER.info("ADD NODE  " + boollit.toString());
            this.configReader.addNode(boollit);
        }
        this.ctx.push(boollit);

    }

    public boolean areString(Collection<Node> nset) {
        for(Node n : nset) {
            if(n.isString())
                continue;

            return false;
        }
        return true;
    }

    public boolean areBool(Collection<Node> nset) {
        for(Node n : nset) {
            if(n.isBoolean())
                continue;

            return false;
        }
        return true;
    }


    public boolean areNum(Collection<Node> nset) {
        for(Node n : nset) {
            if(n.isNumeric())
                continue;

            return false;
        }
        return true;
    }

    private void handleOp() {

        LinkedList<Node> params = this.ctx.getNodesForCtx();

        String sctx = this.ctx.getRecentCxt().getBackref().getName();

        //LOGGER.info("CALLING CTX " + sctx);

        StringPair op = this.ctx.pop(); //op

        LOGGER.debug("OP " + op);

        String type = op.key; // funccall
        String skind = op.value;

        //LOGGER.info("TYPE " + type);
        //LOGGER.info("Kind " + skind);

        String fcall = this.ctx.pop().value; //

        //String fcall = id;

        LOGGER.debug(" BOOO FCALL " + fcall);
        //LOGGER.info("OP " + op);



        //LOGGER.info("PARAMS " + params.size());

        NodeKind okind = NodeKind.KindFromString(skind);

        //LOGGER.info("OKIND " + okind);
        Operation newop = null;

        // infer the types for overloaded operators (==)

        if(okind == NodeKind.EQUALS){
            if(areString(params))
                okind = NodeKind.STR_EQUALS;
            else if(areNum(params))
                okind = NodeKind.NUM_EQUALS;
            else if(areBool(params))
                okind = NodeKind.BOOL_EQUALS;
        } else if (okind == NodeKind.NEQUALS){
            if(areString(params))
                okind = NodeKind.STR_NEQUALS;
            else if(areNum(params))
                okind = NodeKind.NUM_NEQUALS;
            else if(areBool(params))
                okind = NodeKind.BOOL_NEQUALS;
        }

        if(okind == NodeKind.UNKNOWN) {
            newop = this.configReader.getCnetwork().addExtOperation(skind,params);
        } else {
            newop = this.configReader.getCnetwork().addOperation(okind,params);
        }

        LOGGER.info("NEWOP KIND "+ newop.getKind().toString());

        if (newop.getKind() == NodeKind.ITE) {

            assert params.size() == 3;

            assert (params.get(1).isNumeric() && params.get(2).isNumeric()) ||
                    (params.get(1).isString() && params.get(2).isString()) ||
                    (params.get(1).isBoolean() && params.get(2).isBoolean());

            LOGGER.debug("1 ite " + params.get(1).getKind() + " "  + params.get
                    (2).getKind());

            Operation parop = null;

            if(params.get(1).isOperation())
                parop = (Operation)params.get(1);
            else if(params.get(2).isOperation())
                parop = (Operation)params.get(2);

            if(parop != null) {
                alterDomain(newop,params.get(1).getKind
                       ().getDomainKind());
            } else {
                if (params.get(1).isBoolean()) {
                    alterDomain(newop,DomainKind.BOOLEAN);
                } else if (params.get(1).isString()) {
                    alterDomain(newop,DomainKind.STRING);
                }
            }
        }


        // safe node (context can be either term or funccall
        this.ctx.getRecentCxt().getBackref().addNode(newop);
    }

    private void alterDomain(Node n, DomainKind newKind) {
        NodeKind kind = n.getKind();
        try {
            kind.setDomainKind(newKind);
        } catch (IllegalDomainException e) {
            LOGGER.error(e.getMessage());
            System.exit(-1);
        }
        NodeDomain dkind = NodeDomainFactory.INSTANCE.getDomain
                (kind);
        dkind.setDomain(dkind);
    }

    private Operation handleConstraint() {
        //LOGGER.info("HANDLE " + this.ctx.getRecentCxt().getName());
        List<Node> nods = this.ctx.getNodesForCtx();
        //LOGGER.info("SS " + nods.size());
        //assert(nods.size() == 2);

        if(nods.size() == 2) {
            constraint.addNode(nods.get(0));
            constraint.addNode(nods.get(1));

            boolean negate = (constraint.opKind == NodeKind.NEQUALS);
            //LOGGER.debug("+++" + constraint.term0.getKind() + " " + constraint
            //        .term1.getKind());

            //LOGGER.info("OPKIND " + constraint.opKind.getId());
            // type inference
            if(constraint.opKind == NodeKind.EQUALS || negate) {
                if(constraint.isNumeric()) {
                    constraint.setOpKind(NodeKind.NUM_EQUALS);
                } else if (constraint.isString()) {
                    constraint.setOpKind(NodeKind.STR_EQUALS);
                } else if (constraint.isBoolean()) {
                    constraint.setOpKind(NodeKind.BOOL_EQUALS);
                } else {
                    assert(false);
                }
            }
            assert constraint.opKind != null;

            Operation c = this.configReader.addConstraint(constraint);

            if(negate) {
                c.setDomain(NodeDomainFactory.INSTANCE.getDomain
                        (NodeKind.BOOLLIT, "false"));
            }

            return c;

        } else if (nods.size() == 1) {
            switch (constraint.opKind) {
                case EMTPY: return this.configReader.getCnetwork().addConstraint(NodeKind.EMTPY, nods.get(0));
                case NOT: return this.configReader.getCnetwork().addConstraint(NodeKind.NOT, nods.get(0));
            }
        }

        LOGGER.info("KKK 1" + constraint.toString());

        assert false;
        return null;
    }

}