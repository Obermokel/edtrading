package borg.edtrading;

import boofcv.gui.image.VisualizeImageData;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageGray;
import borg.edtrading.bodyscanner.BodyScanner;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.simple.GaussianBlurTransformation;
import borg.edtrading.imagetransformation.simple.KeepBodyScannerTextOnlyTransformation;
import borg.edtrading.imagetransformation.simple.RgbToGrayTransformation;
import borg.edtrading.imagetransformation.simple.ThresholdTransformation;
import borg.edtrading.ocr.CharacterLocator;
import borg.edtrading.ocr.TextBuilder;
import borg.edtrading.ocr.TextLine;
import borg.edtrading.screenshots.Region;
import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Match;
import borg.edtrading.templatematching.Template;
import borg.edtrading.templatematching.TemplateMatcher;
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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

/**
 * FactionScannerApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FactionScannerApp {

    static final Logger logger = LogManager.getLogger(FactionScannerApp.class);

    static CharacterLocator characterLocator = null;
    static List<Template> allTemplates = null;
    static List<Template> alphanumTemplates = null;

    public static void main(String[] args) throws IOException {
        List<File> screenshotFiles = FactionScannerApp.selectAllScreenshots();

        characterLocator = new CharacterLocator(2, 40, 16, 40, 1); // min 2x16, max 40x40, 1px border
        allTemplates = Template.fromFolder("BodyScanner");
        alphanumTemplates = allTemplates.stream().filter(t -> t.getText().matches("[A-Za-z0-9]")).collect(Collectors.toList());

        for (File screenshotFile : screenshotFiles) {
            Screenshot screenshot = Screenshot.loadFromFile(screenshotFile, 3840, 2160, null);
            Region region = screenshot.getAsRegion();
            region.applyTransformation("BSTO", new KeepBodyScannerTextOnlyTransformation());
            region.applyTransformation("GRAY", new RgbToGrayTransformation());
            region.applyTransformation("THRESH", new ThresholdTransformation(128));
            GrayU8 thresholdedImage = (GrayU8) region.getImageData(Transformation.LAST);
            region.applyTransformation("BLUR", new GaussianBlurTransformation(2, -1));
            GrayF32 blurredImage = (GrayF32) region.getImageData(Transformation.LAST);
            List<Rectangle> typicalCharacterSizeLocations = characterLocator.findLocationsOfTypicalCharacterSize(thresholdedImage);
            List<Match> alphanumMatches = new ArrayList<>(typicalCharacterSizeLocations.size());
            for (Rectangle r : typicalCharacterSizeLocations) {
                Region charRegion = region.getSubregion(r.x, r.y, r.width, r.height);
                Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, alphanumTemplates);
                if (bestMatch != null && bestMatch.getErrorPerPixel() <= BodyScanner.ERROR_PER_PIXEL_GUESSED) {
                    alphanumMatches.add(bestMatch);
                } else {
                    List<Match> nonOverlappingMatches = new TemplateMatcher().allNonOverlappingTemplates(charRegion, alphanumTemplates);
                    nonOverlappingMatches = nonOverlappingMatches.stream().filter(m -> m.getErrorPerPixel() <= BodyScanner.ERROR_PER_PIXEL_GUESSED).collect(Collectors.toList());
                    if (nonOverlappingMatches.size() >= 2) {
                        alphanumMatches.addAll(nonOverlappingMatches);
                    }
                }
            }
            List<TextLine> alphanumTextLines = TextBuilder.matchesToText(alphanumMatches);
            List<Rectangle> locationsWithinTextLines = characterLocator.findLocationsWithinTextLines(thresholdedImage, alphanumTextLines);
            List<Match> allMatches = new ArrayList<>(locationsWithinTextLines.size());
            for (Rectangle r : locationsWithinTextLines) {
                Region charRegion = region.getSubregion(r.x, r.y, r.width, r.height);
                Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, allTemplates);
                if (bestMatch != null && bestMatch.getErrorPerPixel() <= BodyScanner.ERROR_PER_PIXEL_UNKNOWN) {
                    allMatches.add(bestMatch);
                } else {
                    List<Match> nonOverlappingMatches = new TemplateMatcher().allNonOverlappingTemplates(charRegion, alphanumTemplates);
                    nonOverlappingMatches = nonOverlappingMatches.stream().filter(m -> m.getErrorPerPixel() <= BodyScanner.ERROR_PER_PIXEL_UNKNOWN).collect(Collectors.toList());
                    if (nonOverlappingMatches.size() >= 2) {
                        allMatches.addAll(nonOverlappingMatches);
                    }
                }
            }
            List<TextLine> allTextLines = TextBuilder.matchesToText(allMatches);
            BufferedImage bi = debugTextLines(region, allTextLines);
            ImageIO.write(bi, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName()));
            for (TextLine tl : allTextLines) {
                logger.debug(tl);
            }
        }
    }

    private static BufferedImage debugTextLines(Region region, List<TextLine> textLines) {
        Random rand = new Random();
        BufferedImage gi = VisualizeImageData.grayMagnitude((ImageGray) region.getImageData(Transformation.LAST), null, -1);
        BufferedImage bi = new BufferedImage(gi.getWidth(), gi.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = bi.createGraphics();
        g.drawImage(gi, 0, 0, null);
        g.setFont(new Font("Consolas", Font.PLAIN, 22));
        for (TextLine tl : textLines) {
            //g.setColor(Color.GRAY);
            g.setColor(new Color(64 + rand.nextInt(128), 64 + rand.nextInt(128), 64 + rand.nextInt(128)));
            g.drawRect(tl.getxInScreenshot(), tl.getyInScreenshot(), tl.getWidth(), tl.getHeight());
            g.setColor(Color.GREEN);
            g.drawString(tl.toText(), tl.getxInScreenshot(), tl.getyInScreenshot());
        }
        logger.debug(region.getScreenshot().getFile().getName() + ": lines=" + textLines.size());

        // Return debug image
        return bi;
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
