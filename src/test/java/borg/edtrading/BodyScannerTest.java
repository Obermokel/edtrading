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
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "LEARNED_FIXED"));
        FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "LEARNED_VARIANT"));

        logger.trace("Creating the scanner...");
        BodyScanner scanner = new BodyScanner();
        scanner.setDebugAlphanumTemplates(false);
        scanner.setDebugAlphanumTextLines(false);
        scanner.setDebugAllTemplates(true);
        scanner.setDebugAllTextLines(true);

        //File sourceFile = selectRandomScreenshot();
        //2016-09-29 08-16-28 Paul-Friedrichs Star
        //2016-10-03 08-37-57 Altair
        //2016-09-29 08-24-03 BD+63 1764
        File sourceFile = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR + "\\2016-09-29 08-24-03 BD+63 1764.png");
        //for (File sourceFile : selectAllScreenshots()) {
        logger.trace("Testing " + sourceFile.getName());
        BodyScannerResult result = scanner.scanScreenshotFile(sourceFile);
        if (result.getAlphanumTemplatesDebugImage() != null) {
            ImageIO.write(result.getAlphanumTemplatesDebugImage(), "PNG", new File(Constants.TEMP_DIR, "AlphanumTemplatesDebugImage " + sourceFile.getName()));
        }
        if (result.getAlphanumTextLinesDebugImage() != null) {
            ImageIO.write(result.getAlphanumTextLinesDebugImage(), "PNG", new File(Constants.TEMP_DIR, "AlphanumTextLinesDebugImage " + sourceFile.getName()));
        }
        if (result.getAllTemplatesDebugImage() != null) {
            ImageIO.write(result.getAllTemplatesDebugImage(), "PNG", new File(Constants.TEMP_DIR, "AllTemplatesDebugImage " + sourceFile.getName()));
        }
        if (result.getAllTextLinesDebugImage() != null) {
            ImageIO.write(result.getAllTextLinesDebugImage(), "PNG", new File(Constants.TEMP_DIR, "AllTextLinesDebugImage " + sourceFile.getName()));
        }

        //            copyLearnedChars(new File(Constants.TEMPLATES_DIR, "LEARNED_FIXED"), new File(Constants.TEMPLATES_DIR, "BodyScanner"));
        //            copyLearnedChars(new File(Constants.TEMPLATES_DIR, "LEARNED_VARIANT"), new File(Constants.TEMPLATES_DIR, "BodyScanner"));
        //            scanner = new BodyScanner();
        //}
    }

    static void copyLearnedChars(File learnedSetDir, File targetSetDir) throws IOException {
        final Random random = new Random(System.currentTimeMillis());
        File[] templateTextDirs = learnedSetDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.isDirectory();
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
