package borg.edtrading;

import borg.edtrading.bodyscanner.BodyScanner;
import borg.edtrading.bodyscanner.BodyScannerResult;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import javax.imageio.ImageIO;

/**
 * BodyScannerTest
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyScannerTest {

    static final Logger logger = LogManager.getLogger(BodyScannerTest.class);

    public static void main(String[] args) throws IOException {
        logger.trace("Cleaning dirs...");
        FileUtils.cleanDirectory(Constants.TEMP_DIR);
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "KNOWN"));
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "UNKNOWN"));
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "GUESSED"));

        logger.trace("Creating the scanner...");
        BodyScanner scanner = new BodyScanner();
        scanner.setDebugAlphanumTemplates(true);
        scanner.setDebugTextLines(true);

        //File sourceFile = selectRandomScreenshot();
        File sourceFile = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR + "\\2016-10-10 05-37-59 Pethes.png");
        //for (File sourceFile : selectAllScreenshots()) {
        logger.trace("Testing " + sourceFile.getName());
        BodyScannerResult result = scanner.scanScreenshotFile(sourceFile);
        if (result.getAlphanumTemplatesDebugImage() != null) {
            ImageIO.write(result.getAlphanumTemplatesDebugImage(), "PNG", new File(Constants.TEMP_DIR, "AlphanumTemplatesDebugImage " + sourceFile.getName()));
        }
        if (result.getTextLinesDebugImage() != null) {
            ImageIO.write(result.getTextLinesDebugImage(), "PNG", new File(Constants.TEMP_DIR, "TextLinesDebugImage " + sourceFile.getName()));
        }
        //}
    }

    static File selectRandomScreenshot() {
        File dir = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR);
        File[] files = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().endsWith(".png");
            }
        });
        Random rand = new Random();
        return files[rand.nextInt(files.length)];
    }

    static List<File> selectAllScreenshots() {
        File dir = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR);
        File[] fileArray = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().endsWith(".png");
            }
        });
        List<File> fileList = new ArrayList<>(fileArray.length);
        for (File f : fileArray) {
            fileList.add(f);
        }
        Collections.shuffle(fileList);
        return fileList;
    }

}
