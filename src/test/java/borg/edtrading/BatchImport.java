package borg.edtrading;

import java.io.File;
import java.io.FileFilter;
import java.util.Map;

import borg.edtrading.boofcv.ScreenshotScanner;
import borg.edtrading.service.TradingService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * BatchImport
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BatchImport {

    static final Logger logger = LogManager.getLogger(BatchImport.class);

    public static void main(String[] args) {
        final TradingService ts = TradingService.getInstance();

        // Set last scan date to null to import all
        ts.setLastScannedScreenshotDate(null);

        File[] screenshotFiles = Constants.SCREENSHOTS_DIR.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().startsWith("elitedangerous64_") && file.getName().endsWith(".png");
            }
        });
        for (File screenshotFile : screenshotFiles) {
            Map<Integer, Map<String, String>> data = ScreenshotScanner.scanScreenshot(screenshotFile);
        }
    }

}
