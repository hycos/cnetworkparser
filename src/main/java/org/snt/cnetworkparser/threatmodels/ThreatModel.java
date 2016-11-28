package org.snt.cnetworkparser.threatmodels;

import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.NodeKind;

import java.util.HashMap;
import java.util.Map;


public abstract class ThreatModel {

    public HashMap<NodeKind, ThreatModel> tmodel = null;
    public ThreatModel() {
        this.tmodel = new HashMap<NodeKind, ThreatModel> ();
    }
    public abstract Map<NodeKind,ThreatModel> getThreatModels();
    public abstract ConstraintNetwork delegate(NodeKind kind);
}
