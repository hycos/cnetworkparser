package org.snt.cnetworkparser.threatmodels;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetworkparser.exception.UnknownException;

import java.util.HashMap;

public class ThreatModelFactory {

    private static ThreatModelFactory instance = null;

    private HashMap<NodeKind, ThreatModel> tmodel = null;


    public static ThreatModelFactory getInstance() {
        if(instance == null)
            instance = new ThreatModelFactory();
        return instance;
    }

    private ThreatModelFactory() {
        this.tmodel = new HashMap<NodeKind, ThreatModel>();
        this.tmodel.putAll(new Ldapi().getThreatModels());
        this.tmodel.putAll(new Sqli().getThreatModels());
        this.tmodel.putAll(new Xmli().getThreatModels());
        this.tmodel.putAll(new Xpathi().getThreatModels());
        this.tmodel.putAll(new Xss().getThreatModels());
        this.tmodel.putAll(new Urli().getThreatModels());
    }

    public ConstraintNetwork getCNforVulnerability(NodeKind kind) throws UnknownException {

        if(!tmodel.containsKey(kind))
            throw new UnknownException("Threat model " + kind + " is not known");


        return tmodel.get(kind).delegate(kind);
    }

}
