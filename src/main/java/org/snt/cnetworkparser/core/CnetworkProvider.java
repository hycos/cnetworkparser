package org.snt.cnetworkparser.core;

import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.inmemantlr.listener.DefaultListener;

public interface CnetworkProvider {
    DefaultListener getListener();
    ConstraintNetwork getConstraintNetwork();
    ConstraintNetworkBuilder getConstraintNetworkBuilder();
}
