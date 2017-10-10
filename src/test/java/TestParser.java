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

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.analytics.CnetworkAnalyzer;
import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;
import com.github.hycos.cnetworkparser.core.ConstraintNetworkParser;
import com.github.hycos.cnetworkparser.core.InputFormat;
import org.snt.inmemantlr.exceptions.CompilationException;

import java.io.File;
import java.util.Set;

public class TestParser {

    final static Logger LOGGER = LoggerFactory.getLogger(TestParser.class);

    private static String getPath(String f){
        ClassLoader classLoader = TestParser.class.getClassLoader();
        File sfile = new File(classLoader.getResource(f).getFile());
        return sfile.getAbsolutePath();
    }

    @Test
    public void testSimpleNum0() {
        ConstraintNetworkBuilder cn = null;
        boolean thrown = false;
        try {
            cn = new ConstraintNetworkParser(InputFormat.Z3STR2).
                    getConstraintNetworkBuilderFromFile(getPath("simplenum" +
                            "0.z3"));
        } catch (EUFInconsistencyException | CompilationException e) {
            thrown = true;
        }

        //LOGGER.debug(cn.getConstraintNetwork().toDot());
        //LOGGER.debug(cn.getEufLattice().toDot());

        Assert.assertFalse(thrown);


        CnetworkAnalyzer.INSTANCE.detectLoopPoints(cn.getConstraintNetwork());

    }

    @Test
    public void testSimpleNum1() {
        ConstraintNetworkBuilder cn = null;
        boolean thrown = false;
        try {
            cn = new ConstraintNetworkParser(InputFormat.Z3STR2).
                    getConstraintNetworkBuilderFromFile(getPath("simplenum" +
                            "1.z3"));
        } catch (EUFInconsistencyException | CompilationException e) {
            thrown = true;
        }

        LOGGER.debug(cn.getConstraintNetwork().toDot());
        LOGGER.debug(cn.getEufLattice().toDot());

        Set<Node> ret = CnetworkAnalyzer.INSTANCE.detectLoopPoints(cn);

        LOGGER.debug("-- {}", ret);

        Assert.assertFalse(thrown);
    }

//    @Test
//    public void testMisc0() {
//        ConstraintNetworkBuilder cn = null;
//        boolean thrown = false;
//        try {
//            cn = new ConstraintNetworkParser(InputFormat.Z3STR2).
//                    getConstraintNetworkBuilderFromFile(getPath("misc/ite0.z3"));
//        } catch (EUFInconsistencyException | CompilationException e) {
//           thrown = true;
//        }
//
//        Assert.assertTrue(thrown);
//    }

    @Test
    public void testSol() {
        ConstraintNetwork cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat
                    .SOL).
                    getConstraintNetworkFromFile(getPath("2.sol"));
        } catch (EUFInconsistencyException | CompilationException e) {
            Assert.assertFalse(true);
        }
    }

    @Test
    public void testCVC() {
        ConstraintNetwork cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.CVC4).
                    getConstraintNetworkFromFile(getPath("1.cvc"));
        }  catch (EUFInconsistencyException | CompilationException e) {
            Assert.assertFalse(true);
        }
        Assert.assertTrue(cn != null);

        //LOGGER.info(cn.toConfig());
    }

    @Test
    public void testS3() {
        ConstraintNetwork cn  = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.S3).
                    getConstraintNetworkFromFile(getPath("1.s3"));
        } catch (EUFInconsistencyException | CompilationException e) {
            Assert.assertFalse(true);
        }
        Assert.assertTrue(cn != null);
    }

    @Test
    public void testZ3() {
        ConstraintNetworkBuilder cn  = null;

        String s = getPath("pisa/pisa-002.z3");
        LOGGER.debug("s {}",s);

        try {
            cn = new ConstraintNetworkParser(InputFormat.Z3STR2).
                    getConstraintNetworkBuilderFromFile(s);
        } catch (EUFInconsistencyException | CompilationException e) {
            Assert.assertFalse(true);
        }
        LOGGER.debug(cn.getConstraintNetwork().toDot());
        LOGGER.debug(cn.getEufLattice().toDot());
    }

    @Test
    public void testSamples() {
        ConstraintNetwork cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat
                    .SOL).
                    getConstraintNetworkFromFile(getPath("simple01.sol"));
        } catch (EUFInconsistencyException | CompilationException e) {
            Assert.assertFalse(true);
        }
        LOGGER.debug(cn.toDot());
        Assert.assertTrue(cn != null);

    }

    @Test
    public void testZ3Num(){
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat
                    .Z3STR2).
                    getConstraintNetworkBuilderFromFile(getPath("z3num.z3"));
        } catch (EUFInconsistencyException | CompilationException e) {
            Assert.assertFalse(true);
        }
        LOGGER.debug("euf");
        LOGGER.debug(cn.getEufLattice().toDot());
    }

    @Test
    public void testZ3Lencheck(){
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat
                    .Z3STR2).
                    getConstraintNetworkBuilderFromFile(getPath("lencheck.z3"));
        } catch (EUFInconsistencyException | CompilationException e) {
            Assert.assertFalse(true);
        }
        LOGGER.debug("euf");
        LOGGER.debug(cn.getConstraintNetwork().toDot());
        LOGGER.debug(cn.getEufLattice().toDot());
    }

    @Test
    public void testKaluzaEUF() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.Z3STR2).
                    getConstraintNetworkBuilderFromFile(getPath("kaluza4.z3"));
        } catch (EUFInconsistencyException | CompilationException e) {
            Assert.assertFalse(true);
        }

        LOGGER.debug(cn.getEufLattice().toDot());
    }

    @Test
    public void testIdxOfEUF() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.Z3STR2).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("pisa-004t.z3"));
        } catch (EUFInconsistencyException | CompilationException e) {
            Assert.assertFalse(true);
        }
        CnetworkAnalyzer.INSTANCE.checkForCycles(cn);
        LOGGER.debug(cn.getEufLattice().toDot());
    }


    @Test
    public void testAuthorizationServlet() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("AuthorizationServlet0.sol"));
        } catch (EUFInconsistencyException | CompilationException e) {
            Assert.assertFalse(true);
        }
        //CnetworkAnalyzer.INSTANCE.checkForCycles(cn);
        LOGGER.debug(cn.getConstraintNetwork().toDot());
    }


}