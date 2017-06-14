package org.snt.cnetworkparser.threatmodels;

import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.graph.NodeKind;

import java.util.HashMap;
import java.util.Map;


public abstract class ThreatModel {

    public HashMap<NodeKind, ThreatModel> tmodel = null;
    public ThreatModel() {
        this.tmodel = new HashMap<> ();
    }
    public abstract Map<NodeKind,ThreatModel> getThreatModels();
    public abstract ConstraintNetworkBuilder delegate(NodeKind kind);
}
