package com.github.hycos.cnetworkparser.core;

import com.github.hycos.cnetwork.api.NodeKindFactoryInterface;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.DefaultNodeKindFactory;

public class DefaultConstraintNetworkBuilderFactory implements ConstraintNetworkBuilderFactoryInterface {

    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() {
        return new ConstraintNetworkBuilder();
    }

    @Override
    public NodeKindFactoryInterface getNodeKindFactory() {
        return DefaultNodeKindFactory.INSTANCE;
    }

}
