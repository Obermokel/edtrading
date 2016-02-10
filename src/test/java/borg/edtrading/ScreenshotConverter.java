package borg.edtrading;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;

import boofcv.io.image.UtilImageIO;
import borg.edtrading.boofcv.ScreenshotScanner;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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
            BufferedImage croppedImage = ScreenshotScanner.crop3440x1440(originalImage);
            BufferedImage orangeTextImage = ScreenshotScanner.keepOrangeTextOnly(croppedImage);
            UtilImageIO.saveImage(orangeTextImage, orangeTextFile.getAbsolutePath());
        }
    }

}
