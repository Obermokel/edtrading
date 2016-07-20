package borg.edtrading;

import boofcv.io.image.UtilImageIO;
import borg.edtrading.boofcv.ScreenshotScanner;
import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatcher;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;
import java.util.List;

/**
 * ScreenshotConverter
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScreenshotConverter {

    static final Logger logger = LogManager.getLogger(ScreenshotConverter.class);

    public static void main(String[] args) {
        File[] screenshotFiles = Constants.SCREENSHOTS_DIR.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().startsWith("elitedangerous64_") && file.getName().endsWith(".png");
            }
        });
        for (File screenshotFile : screenshotFiles) {
            File orangeTextFile = new File(screenshotFile.getParentFile(), "orangeText_" + screenshotFile.getName());
            BufferedImage originalImage = UtilImageIO.loadImage(screenshotFile.getAbsolutePath());
            BufferedImage orangeTextImage = ScreenshotScanner.keepOrangeTextOnly(originalImage);
            List<Template> templates = TemplateMatcher.loadTemplates();
            BufferedImage croppedImage = ScreenshotScanner.cropToCommoditiesMarket(orangeTextImage, templates);
            UtilImageIO.saveImage(croppedImage, orangeTextFile.getAbsolutePath());
        }
    }

}
