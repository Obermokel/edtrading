package borg.edtrading;

import borg.edtrading.ocr.CharacterLocator;
import borg.edtrading.ocr.OcrExecutor;
import borg.edtrading.ocr.OcrResult;
import borg.edtrading.ocr.OcrTask;
import borg.edtrading.ocr.TextLine;
import borg.edtrading.screenshots.Region;
import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Template;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * FactionScannerApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FactionScannerApp {

    static final Logger logger = LogManager.getLogger(FactionScannerApp.class);

    public static void main(String[] args) throws IOException {
        List<File> screenshotFiles = FactionScannerApp.selectAllScreenshots();

        CharacterLocator characterLocator = new CharacterLocator(2, 40, 16, 40, 1); // min 2x16, max 40x40, 1px border
        List<Template> templates = Template.fromFolder("BodyScanner");

        for (File screenshotFile : screenshotFiles) {
            Screenshot screenshot = Screenshot.loadFromFile(screenshotFile, 3840, 2160, null);
            Region region = screenshot.getAsRegion();
            //x=0,y=350,w=840,h=1620

            OcrTask ocrTask = new OcrTask(region, characterLocator, templates);
            ocrTask.setDebugAllTextLines(true);
            OcrResult ocrResult = new OcrExecutor().executeOcr(ocrTask);
            ocrResult.writeDebugImages();
            for (TextLine tl : ocrResult.getTextLines()) {
                System.out.println(tl);
            }
        }
    }

    static List<File> selectSpecificScreenshot(String filename) {
        return Arrays.asList(new File(Constants.FACTION_SCREENSHOTS_DIR, filename));
    }

    static List<File> selectRandomScreenshot() {
        return selectAllScreenshots().subList(0, 1);
    }

    static List<File> selectAllScreenshots() {
        File[] fileArray = Constants.FACTION_SCREENSHOTS_DIR.listFiles(new FileFilter() {
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
