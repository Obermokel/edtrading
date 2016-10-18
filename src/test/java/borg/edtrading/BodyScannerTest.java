package borg.edtrading;

import borg.edtrading.imagetransformation.BodyScannerFeatureLocatorTransformation;
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

        Screenshot screenshot1080 = null;
        Screenshot screenshot2160 = null;

        //File sourceFile = selectRandomScreenshot();
        //File sourceFile = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR + "\\2016-09-28 07-47-30 LHS 380.png");
        for (File sourceFile : selectAllScreenshots()) {

            // Load the screenshot in FullHD resolution for fast location of the relevant areas
            screenshot1080 = Screenshot.loadFromFile(sourceFile, 1920, 1080, screenshot1080);
            logger.trace("Testing " + screenshot1080);
            //screenshot.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".00-Screenshot.png")));

            // Convert to gray image and apply a simple threshold
            Region region1080 = screenshot1080.getAsRegion();
            //region720.applyTransformation(new RgbToGrayTransformation()).applyTransformation(new ThresholdingTransformation(64));
            //region1080.applyTransformation(new RgbToGrayTransformation()).applyTransformation(new CannyEdgeTransformation(2, 0.033f, 0.1f));
            //@formatter:off
            region1080.applyTransformation(new BodyScannerFeatureLocatorTransformation())
                    .saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".BSFL.png")), Transformation.LAST);
            //@formatter:on
            //region1080.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".canny.png")), Transformation.LAST);
            //region720.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".original.png")), Transformation.ORIGINAL);

            //            // Get the body data region
            //            Region bodyDataRegion = screenshot720.getRegion(20, 340, 820, 1690);
            //            //bodyDataRegion.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".10-BodyDataRegion.png")), Transformation.ORIGINAL);
            //
            //            // Keep only white text
            //            bodyDataRegion.applyTransformation(new KeepBodyScannerTextOnlyTransformation());
            //            bodyDataRegion.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".20-" + KeepBodyScannerTextOnlyTransformation.class.getSimpleName() + ".png")), KeepBodyScannerTextOnlyTransformation.class.getSimpleName());

        }
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
