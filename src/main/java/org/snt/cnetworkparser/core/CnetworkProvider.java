package org.snt.cnetworkparser.core;

import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.inmemantlr.DefaultListener;

public interface CnetworkProvider {
    DefaultListener getListener();
    ConstraintNetwork getConstraintNetwork();
}
