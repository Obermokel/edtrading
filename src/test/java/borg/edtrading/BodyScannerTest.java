package borg.edtrading;

import boofcv.gui.image.VisualizeImageData;
import boofcv.struct.image.GrayU8;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.combined.BodyScannerCharMatchingTransformation;
import borg.edtrading.ocr.CharacterLocator;
import borg.edtrading.ocr.TextLine;
import borg.edtrading.screenshots.Region;
import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Match;
import borg.edtrading.templatematching.Template;
import borg.edtrading.templatematching.TemplateMatcher;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
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
        List<Template> templates = Template.fromFolder("BodyScanner");

        Screenshot screenshot1080 = null;
        Screenshot screenshot2160 = null;

        //File sourceFile = selectRandomScreenshot();
        File sourceFile = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR + "\\2016-10-10 04-35-07 Wolf 25.png");
        //for (File sourceFile : selectAllScreenshots()) {

        logger.trace("Testing " + sourceFile.getName());
        //writeGuessedOrUnknownTemplates(sourceFile, templates);

        //        // Load the screenshot in FullHD resolution for fast location of the relevant areas
        //        screenshot1080 = Screenshot.loadFromFile(sourceFile, 1920, 1080, screenshot1080);
        //        Region region1080 = screenshot1080.getAsRegion();
        //        region1080.applyTransformation(new BodyScannerFeatureLocatorTransformation());
        //        //region1080.saveToFile(new File(Constants.TEMP_DIR, sourceFile.getName().replace(".png", ".BSFL.png")), Transformation.LAST);

        // Load the screenshot in 4K resolution for template matching of chars
        screenshot2160 = Screenshot.loadFromFile(sourceFile, 3840, 2160, screenshot2160);

        Region region2160sharp = screenshot2160.getAsRegion();
        region2160sharp.applyTransformation(new BodyScannerCharMatchingTransformation(false));
        List<Rectangle> rects = new CharacterLocator(2, 40, 16, 40, 1).locateCharacters((GrayU8) region2160sharp.getImageData(Transformation.LAST));

        List<Match> matches = new ArrayList<>(rects.size());
        Region region2160blurred = screenshot2160.getAsRegion();
        region2160blurred.applyTransformation(new BodyScannerCharMatchingTransformation(true));
        for (Rectangle r : rects) {
            Region charRegion = region2160blurred.getSubregion(r.x, r.y, r.width, r.height);
            Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, templates);
            if (bestMatch.getErrorPerPixel() <= 0.05f) {
                matches.add(bestMatch);
            }
        }
        TextLine.matchesToTextLines(matches);
        //}
    }

    private static void writeGuessedOrUnknownTemplates(File sourceFile, List<Template> knownTemplates) throws IOException {
        Screenshot screenshot2160 = Screenshot.loadFromFile(sourceFile, 3840, 2160, null);

        // Detect char locations
        Region region2160sharp = screenshot2160.getAsRegion();
        region2160sharp.applyTransformation(new BodyScannerCharMatchingTransformation(false));
        List<Rectangle> rects = new CharacterLocator(2, 40, 16, 40, 1).locateCharacters((GrayU8) region2160sharp.getImageData(Transformation.LAST));

        BufferedImage gi = VisualizeImageData.grayMagnitude((GrayU8) region2160sharp.getImageData(Transformation.LAST), null, -1);
        BufferedImage bi = new BufferedImage(gi.getWidth(), gi.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = bi.createGraphics();
        g.drawImage(gi, 0, 0, null);
        g.setFont(new Font("Consolas", Font.PLAIN, 22));
        g.setColor(Color.GRAY);
        for (Rectangle r : rects) {
            g.drawRect(r.x, r.y, r.width, r.height);
        }

        // Match chars
        Region region2160blurred = screenshot2160.getAsRegion();
        region2160blurred.applyTransformation(new BodyScannerCharMatchingTransformation(true));
        int nKnown = 0;
        int nGuessed = 0;
        int nUnknown = 0;
        for (Rectangle r : rects) {
            Region charRegion = region2160blurred.getSubregion(r.x, r.y, r.width, r.height);
            Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, knownTemplates);
            if (bestMatch.getErrorPerPixel() <= 0.015f) {
                nKnown++;
                Template.createNewFromRegion(charRegion, "KNOWN", bestMatch.getTemplate().getText());
                g.setColor(Color.GREEN);
                g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
            } else if (bestMatch.getErrorPerPixel() <= 0.05f) {
                nGuessed++;
                Template.createNewFromRegion(charRegion, "GUESSED", bestMatch.getTemplate().getText());
                g.setColor(Color.YELLOW);
                g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
            } else {
                nUnknown++;
                Template.createNewFromRegion(charRegion, "UNKNOWN", "UNKNOWN");
                g.setColor(Color.RED);
                g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
            }
        }

        g.dispose();
        ImageIO.write(bi, "png", new File(Constants.TEMP_DIR, "temp.png"));

        logger.debug(screenshot2160.getFile().getName() + ": known=" + nKnown + "; guessed=" + nGuessed + "; unknown=" + nUnknown);
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
