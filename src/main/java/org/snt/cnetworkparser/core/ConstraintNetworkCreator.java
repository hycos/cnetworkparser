package org.snt.cnetworkparser.core;

import org.snt.inmemantlr.listener.DefaultTreeListener;


public abstract class ConstraintNetworkCreator extends DefaultTreeListener implements
        ConstraintNetworkProvider {

    protected boolean eufEnabled = false;

    public ConstraintNetworkCreator(boolean eufEnabled) {
        this.eufEnabled = eufEnabled;
    }
}
