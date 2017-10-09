package com.github.hycos.cnetworkparser.core;

import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;
import org.snt.inmemantlr.listener.DefaultListener;

public interface ConstraintNetworkProvider {
    DefaultListener getListener();
    ConstraintNetwork getConstraintNetwork() throws EUFInconsistencyException;
    ConstraintNetworkBuilder getConstraintNetworkBuilder() throws EUFInconsistencyException;
}
