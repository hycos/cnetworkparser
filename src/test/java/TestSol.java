import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetworkparser.core.ConstraintNetworkParser;
import com.github.hycos.cnetworkparser.core.InputFormat;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.exceptions.CompilationException;

import java.io.File;

public class TestSol {

    final static Logger LOGGER = LoggerFactory.getLogger(TestSol.class);

    private static String getPath(String f){
        ClassLoader classLoader = TestParser.class.getClassLoader();
        File sfile = new File(classLoader.getResource(f).getFile());
        return sfile.getAbsolutePath();
    }


    @Test
    public void advanced0() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/advanced0.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }

    @Test
    public void basket0() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/basket0.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void basket1() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/basket1.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void basket2() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/basket2.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void basket3() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/basket3.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void contact0() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/contact0.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void contact1() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/contact1.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void contact2() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/contact2.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void login0() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/login0.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void login1() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/login1.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void login2() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/login2.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void login3() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/login3.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void login4() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/login4.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void login5() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/login5.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void password0() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/password0.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void password1() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/password1.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void register0() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/register0.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void register1() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/register1.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void search0() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/search0.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }
    @Test
    public void search1() {
        ConstraintNetworkBuilder cn = null;
        try {
            cn = new ConstraintNetworkParser(InputFormat.SOL).
                    getConstraintNetworkBuilderFromFile(getPath
                            ("sol/search1.sol"));
        } catch (InconsistencyException | CompilationException e) {
            LOGGER.debug(e.getMessage());
            Assertions.assertFalse(true);
        }
    }

}
