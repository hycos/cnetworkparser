package org.snt.cnetworkparser.core;

import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.inmemantlr.listener.DefaultListener;

public interface ConstraintNetworkProvider {
    DefaultListener getListener();
    ConstraintNetwork getConstraintNetwork() throws EUFInconsistencyException;
    ConstraintNetworkBuilder getConstraintNetworkBuilder() throws EUFInconsistencyException;
}
