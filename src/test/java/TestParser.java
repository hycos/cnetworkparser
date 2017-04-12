import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetworkparser.core.ConstraintNetworkParser;
import org.snt.cnetworkparser.core.InputFormat;
import org.snt.inmemantlr.exceptions.CompilationException;

import java.io.File;

public class TestParser {

    final static Logger LOGGER = LoggerFactory.getLogger(TestParser.class);

    private static String getPath(String f){
        ClassLoader classLoader = TestParser.class.getClassLoader();
        File sfile = new File(classLoader.getResource(f).getFile());
        return sfile.getAbsolutePath();
    }

    @Test
    public void testMisc0() {
        ConstraintNetworkBuilder cn = null;
        boolean thrown = false;
        try {
            cn = new ConstraintNetworkParser(InputFormat.Z3STR2).
                    getConstraintNetworkBuilderFromFile(getPath("misc/ite0.z3"));
        } catch (EUFInconsistencyException | CompilationException e) {
           thrown = true;
        }

        Assert.assertTrue(thrown);
    }

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
        LOGGER.info(cn.toDot());
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
        LOGGER.info("euf");
        LOGGER.info(cn.getEufLattice().toDot());
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
        LOGGER.info("euf");
        LOGGER.debug(cn.getConstraintNetwork().toDot());
        LOGGER.info(cn.getEufLattice().toDot());
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

        LOGGER.debug(cn.getEufLattice().toDot());
    }


}