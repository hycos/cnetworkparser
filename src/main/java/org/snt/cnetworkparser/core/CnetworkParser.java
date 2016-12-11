package org.snt.cnetworkparser.core;

import org.antlr.v4.runtime.ParserRuleContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.exceptions.IllegalWorkflowException;
import org.snt.inmemantlr.listener.DefaultListener;
import org.snt.inmemantlr.utils.FileUtils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

public class CnetworkParser {

    final static Logger LOGGER = LoggerFactory.getLogger(CnetworkParser.class);

    private GenericParser gp = null;
    private ParserRuleContext ctx = null;
    private DefaultListener rl = null;
    private InputFormat inputFormat;
    private CnetworkProvider provider = null;


    public CnetworkParser(InputFormat inputFormat) {

        this.inputFormat = inputFormat;

        ClassLoader classLoader = getClass().getClassLoader();
        InputStream is = classLoader.getResourceAsStream(inputFormat.getGrammar());
        String s = FileUtils.getStringFromStream(is);

        this.gp = new GenericParser(s);
        this.provider = inputFormat.getProvider();
        this.ctx = null;
        this.gp.setListener(provider.getListener());
        this.gp.compile();
    }

    public ConstraintNetwork getCNfromFile(String path) {

        byte[] encoded = null;
        try {
            encoded = Files.readAllBytes(Paths.get(path));
        } catch (IOException e) {
            return null;
        }

        String s = new String(encoded);

        try {
            this.gp.parse(s);
        } catch (IllegalWorkflowException e) {
            e.printStackTrace();
        }

        ConstraintNetwork cn = this.provider.getConstraintNetwork();
        cn.buildNodeIdx();
        return cn;
    }





}

