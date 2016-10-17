package borg.edtrading;

import borg.edtrading.imagetransformation.KeepBodyScannerTextOnlyTransformation;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.ocr.Region;
import borg.edtrading.ocr.Screenshot;
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

/**
 * BodyScannerTest
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyScannerTest {

    static final Logger logger = LogManager.getLogger(BodyScannerTest.class);

    public static void main(String[] args) throws IOException {
        logger.trace("Cleaning temp dir...");
        FileUtils.cleanDirectory(Constants.TEMP_DIR);

        Screenshot screenshot = null;
        File sourceFile = selectRandomScreenshot();
        //File sourceFile = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR + "\\2016-10-01 20-50-38 HIP 30953.png");
        //for (File sourceFile : selectAllScreenshots()) {
        logger.trace("Loading " + sourceFile);
        screenshot = Screenshot.loadFromFile(sourceFile, 3840, 2160, screenshot);
        logger.trace("Testing " + screenshot);
        screenshot.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".00-Screenshot.png")));
        Region bodyDataRegion = screenshot.getRegion(20, 340, 820, 1690);
        bodyDataRegion.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".10-BodyDataRegion.png")), Transformation.ORIGINAL);
        bodyDataRegion.applyTransformation(new KeepBodyScannerTextOnlyTransformation());
        bodyDataRegion.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".20-" + KeepBodyScannerTextOnlyTransformation.class.getSimpleName() + ".png")), KeepBodyScannerTextOnlyTransformation.class.getSimpleName());
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
