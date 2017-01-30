package org.snt.cnetworkparser.core;

import org.snt.inmemantlr.listener.DefaultTreeListener;


public abstract class CnetworkCreator extends DefaultTreeListener implements
        CnetworkProvider {
    protected boolean eufEnabled = false;
    public CnetworkCreator(boolean eufEnabled) {
        this.eufEnabled = eufEnabled;
    }
}
