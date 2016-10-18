package borg.edtrading;

import boofcv.struct.image.GrayU8;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.combined.BodyScannerFeatureLocatorTransformation;
import borg.edtrading.imagetransformation.simple.GaussianBlurTransformation;
import borg.edtrading.imagetransformation.simple.KeepBodyScannerTextOnlyTransformation;
import borg.edtrading.imagetransformation.simple.RgbToGrayTransformation;
import borg.edtrading.imagetransformation.simple.ThresholdTransformation;
import borg.edtrading.ocr.CharacterLocator;
import borg.edtrading.screenshots.Region;
import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Template;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Rectangle;
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
        File sourceFile = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR + "\\2016-09-28 07-47-30 LHS 380.png");
        //for (File sourceFile : selectAllScreenshots()) {

        // Load the screenshot in FullHD resolution for fast location of the relevant areas
        screenshot1080 = Screenshot.loadFromFile(sourceFile, 1920, 1080, screenshot1080);
        logger.trace("Testing " + screenshot1080);
        //screenshot.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".00-Screenshot.png")));

        // Convert to gray image and apply a simple threshold
        Region region1080 = screenshot1080.getAsRegion();
        region1080.applyTransformation(new BodyScannerFeatureLocatorTransformation());
        region1080.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".BSFL.png")), Transformation.LAST);

        // Load the screenshot in 4K resolution for template matching of chars
        screenshot2160 = Screenshot.loadFromFile(sourceFile, 3840, 2160, screenshot2160);
        Region region2160 = screenshot2160.getAsRegion();
        region2160.applyTransformation(new KeepBodyScannerTextOnlyTransformation());
        region2160.applyTransformation(new RgbToGrayTransformation());
        region2160.applyTransformation(new ThresholdTransformation(128));

        // Detect char locations
        List<Rectangle> rects = new CharacterLocator(2, 20, 3, 30, 1).locateCharacters((GrayU8) region2160.getImageData(Transformation.LAST));

        // Add blur and save templates
        region2160.applyTransformation(new GaussianBlurTransformation(2, -1));
        for (Rectangle r : rects) {
            Region charRegion = region2160.getSubregion(r.x, r.y, r.width, r.height);
            Template template = Template.createNewFromRegion(charRegion, "UNKNOWN", "UNKNOWN");
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
