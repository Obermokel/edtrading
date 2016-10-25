package borg.edtrading;

import borg.edtrading.bodyscanner.BodyScanner;
import borg.edtrading.bodyscanner.BodyScannerResult;
import borg.edtrading.util.ImageUtil;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
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

    private static final DateFormat DF_ETA = new SimpleDateFormat("MMM dd @ HH:mm");

    public static void main(String[] args) throws IOException {
        logger.trace("Cleaning dirs...");
        FileUtils.cleanDirectory(Constants.TEMP_DIR);
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "KNOWN"));
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "UNKNOWN"));
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "GUESSED"));
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "CRAP"));
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "LEARNED_FIXED"));
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "LEARNED_VARIANT"));

        //2016-09-29 08-16-28 Paul-Friedrichs Star
        //2016-10-03 08-37-57 Altair
        //2016-09-29 08-24-03 BD+63 1764
        List<File> screenshotFiles = BodyScannerApp.selectSpecificScreenshot("2016-09-30 17-21-51 Deciat.png");
        //List<File> screenshotFiles = BodyScannerApp.selectRandomScreenshot();
        //List<File> screenshotFiles = BodyScannerApp.selectAllScreenshots();
        int n = 0;
        int total = screenshotFiles.size();
        int batchSize = 10;
        long startBatch = System.currentTimeMillis();
        for (File screenshotFile : screenshotFiles) {
            // Print ETA
            if (++n % batchSize == 0) {
                long millis = System.currentTimeMillis() - startBatch;
                double screenshotsPerSec = ((double) batchSize / Math.max(1, millis)) * 1000d;
                int screenshotsRemaining = total - n;
                double secondsRemaining = screenshotsRemaining / screenshotsPerSec;
                Date eta = new Date(System.currentTimeMillis() + (long) (secondsRemaining * 1000));
                logger.trace(String.format("Processed %,d of %,d screenshots (%.1f/min) -- ETA %s", n, total, screenshotsPerSec * 60, DF_ETA.format(eta)));
                startBatch = System.currentTimeMillis();
            }

            logger.trace("Testing " + screenshotFile.getName());
            BodyScanner scanner = new BodyScanner();
            scanner.setDebugAlphanumTemplates(true);
            scanner.setDebugAlphanumTextLines(true);
            scanner.setDebugAllTemplates(false);
            scanner.setDebugAllTextLines(true);
            scanner.setDebugFinal(true);
            BodyScannerResult result = scanner.scanScreenshotFile(screenshotFile);
            if (result.getAlphanumTemplatesDebugImage() != null) {
                ImageIO.write(result.getAlphanumTemplatesDebugImage(), "PNG", new File(Constants.TEMP_DIR, "AlphanumTemplatesDebugImage " + screenshotFile.getName()));
            }
            if (result.getAlphanumTextLinesDebugImage() != null) {
                ImageIO.write(result.getAlphanumTextLinesDebugImage(), "PNG", new File(Constants.TEMP_DIR, "AlphanumTextLinesDebugImage " + screenshotFile.getName()));
            }
            if (result.getAllTemplatesDebugImage() != null) {
                ImageIO.write(result.getAllTemplatesDebugImage(), "PNG", new File(Constants.TEMP_DIR, "AllTemplatesDebugImage " + screenshotFile.getName()));
            }
            if (result.getAllTextLinesDebugImage() != null) {
                ImageIO.write(result.getAllTextLinesDebugImage(), "PNG", new File(Constants.TEMP_DIR, "AllTextLinesDebugImage " + screenshotFile.getName()));
            }
            if (result.getFinalDebugImage() != null) {
                ImageIO.write(ImageUtil.toFullHd(result.getFinalDebugImage()), "PNG", new File(Constants.TEMP_DIR, "FinalDebugImage " + screenshotFile.getName()));
            }

            copyLearnedChars(new File(Constants.TEMPLATES_DIR, "LEARNED_FIXED"), new File(Constants.TEMPLATES_DIR, "BodyScanner"));
            //FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "LEARNED_FIXED"));
            copyLearnedChars(new File(Constants.TEMPLATES_DIR, "LEARNED_VARIANT"), new File(Constants.TEMPLATES_DIR, "BodyScanner"));
        }
    }

    static void copyLearnedChars(File learnedSetDir, File targetSetDir) throws IOException {
        final Random random = new Random(System.currentTimeMillis());
        File[] templateTextDirs = learnedSetDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                if (learnedSetDir.getName().contains("FIXED")) {
                    return file.isDirectory() && (file.getName().equals("D") || file.getName().equals("_punkt") || file.getName().equals("_komma") || file.getName().equals("_strich") || file.getName().equals("_apostroph"));
                } else {
                    return file.isDirectory();
                }
            }
        });
        if (templateTextDirs != null) {
            for (File templateTextDir : templateTextDirs) {
                // Learn max 1 per type
                File[] pngFiles = templateTextDir.listFiles(new FileFilter() {
                    @Override
                    public boolean accept(File file) {
                        return file.getName().endsWith(".png");
                    }
                });
                if (pngFiles != null && pngFiles.length >= 1) {
                    File randomPngFile = pngFiles[random.nextInt(pngFiles.length)];
                    File targetFile = new File(targetSetDir, templateTextDir.getName() + "/LEARNED#" + randomPngFile.getName());
                    FileUtils.copyFile(randomPngFile, targetFile);
                }
            }
            // Clean learned
            FileUtils.cleanDirectory(learnedSetDir);
        }
    }

}
