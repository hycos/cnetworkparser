/*
 * cnetworkparser - generate constraint network from different formats
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetworkparser is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetworkparser.core;

import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetworkparser.exception.ParserRuntimeException;
import org.antlr.v4.runtime.ParserRuleContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.exceptions.CompilationException;
import org.snt.inmemantlr.exceptions.IllegalWorkflowException;
import org.snt.inmemantlr.exceptions.ParsingException;
import org.snt.inmemantlr.listener.DefaultListener;
import org.snt.inmemantlr.utils.FileUtils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

public class ConstraintNetworkParser {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkParser.class);

    private GenericParser gp = null;
    private ParserRuleContext ctx = null;
    private DefaultListener rl = null;
    private InputFormat inputFormat;
    private ConstraintNetworkGenerator provider = null;


    public ConstraintNetworkParser(InputFormat inputFormat,
                                   ConstraintNetworkBuilderFactoryInterface f)
            throws
            CompilationException {

        this.inputFormat = inputFormat;

        ClassLoader classLoader = getClass().getClassLoader();
        InputStream is = classLoader.getResourceAsStream(inputFormat.getGrammar());
        String s = FileUtils.getStringFromStream(is);

//        if(inputFormat == InputFormat.Z3STR2) {
//            try {
//                this.gp = GenericParser.load("/tmp/z3parser");
//            } catch (DeserializationException e) {
//                e.printStackTrace();
//                System.exit(-1);
//            }
//        } else {
            this.gp = new GenericParser(s);
//        }

        this.provider = inputFormat.getProvider(f);
        this.ctx = null;
        this.gp.setListener(provider.getListener());
        this.gp.compile();
    }

    public ConstraintNetworkParser(InputFormat inputFormat)
            throws
            CompilationException {

        this.inputFormat = inputFormat;

        ClassLoader classLoader = getClass().getClassLoader();
        InputStream is = classLoader.getResourceAsStream(inputFormat.getGrammar());
        String s = FileUtils.getStringFromStream(is);

        this.gp = new GenericParser(s);
        this.provider = inputFormat.getProvider(new DefaultConstraintNetworkBuilderFactory());
        this.ctx = null;
        this.gp.setListener(provider.getListener());
        this.gp.compile();
    }




    public ConstraintNetwork getConstraintNetworkFromFile(String path) throws InconsistencyException {

        byte[] encoded = null;
        try {
            encoded = Files.readAllBytes(Paths.get(path));
        } catch (IOException e) {
            return null;
        }

        String s = new String(encoded);

        try {
            this.gp.parse(s, "s", GenericParser.CaseSensitiveType.NONE);
        } catch (IllegalWorkflowException | ParsingException | ParserRuntimeException e) {
            throw new InconsistencyException(e.getMessage());
        }
        ConstraintNetwork cn = this.provider.getConstraintNetwork();
        //cn.buildNodeIdx();
        return cn;
    }

    public ConstraintNetworkBuilder getConstraintNetworkBuilderFromFile(String path)
            throws InconsistencyException {

        byte[] encoded = null;
        try {
            encoded = Files.readAllBytes(Paths.get(path));
        } catch (IOException e) {
            return null;
        }

        String s = new String(encoded);

        try {
            this.gp.parse(s, "s", GenericParser.CaseSensitiveType.NONE);
        } catch (IllegalWorkflowException | ParsingException | ParserRuntimeException e) {
            throw new InconsistencyException(e.getMessage());
        }
        return this.provider.getConstraintNetworkBuilder();
    }





}

