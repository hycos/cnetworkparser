package com.github.hycos.cnetworkparser.core;

import com.github.hycos.cnetwork.api.NodeKindFactoryInterface;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;

public interface ConstraintNetworkBuilderFactoryInterface {
    ConstraintNetworkBuilder getConstraintNetworkBuilder();
    NodeKindFactoryInterface getNodeKindFactory();
}
