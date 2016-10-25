package borg.edtrading.bodyscanner;

import boofcv.gui.image.VisualizeImageData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageGray;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.simple.GaussianBlurTransformation;
import borg.edtrading.imagetransformation.simple.KeepBodyScannerTextOnlyTransformation;
import borg.edtrading.imagetransformation.simple.RgbToGrayTransformation;
import borg.edtrading.imagetransformation.simple.ThresholdTransformation;
import borg.edtrading.ocr.CharacterLocator;
import borg.edtrading.ocr.TextBuilder;
import borg.edtrading.ocr.TextLine;
import borg.edtrading.ocr.Word;
import borg.edtrading.screenshots.Region;
import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Match;
import borg.edtrading.templatematching.Template;
import borg.edtrading.templatematching.TemplateMatcher;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

/**
 * BodyScanner
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyScanner {

    /**
     * If error/pixel is less or equal than this value we should be 99.9% safe that it has been detected correctly.
     */
    public static final float ERROR_PER_PIXEL_KNOWN = 0.015f;
    /**
     * If error/pixel is less or equal than this value the detection quality should be good enough for only slight
     * errors which can be corrected with levenshtein and known texts.
     */
    public static final float ERROR_PER_PIXEL_GUESSED = 0.040f;
    /**
     * If error/pixel is less or equal than this value we assume that the pixels represent an unknown char. If
     * error/pixel is higher it is likely to be crap.
     */
    public static final float ERROR_PER_PIXEL_UNKNOWN = 0.075f;

    static final Logger logger = LogManager.getLogger(BodyScanner.class);

    private final CharacterLocator characterLocator;
    private final List<Template> allTemplates;
    private final List<Template> alphanumTemplates;

    private boolean debugAlphanumTemplates = false;
    private boolean debugAlphanumTextLines = false;
    private boolean debugAllTemplates = false;
    private boolean debugAllTextLines = false;
    private boolean debugFinal = false;

    public BodyScanner() throws IOException {
        this.characterLocator = new CharacterLocator(2, 40, 16, 40, 1); // min 2x16, max 40x40, 1px border
        this.allTemplates = Template.fromFolder("BodyScanner");
        this.alphanumTemplates = Template.fromFolder("BodyScanner").stream().filter(t -> t.getText().matches("[A-Za-z0-9]")).collect(Collectors.toList());
    }

    public BodyScannerResult scanScreenshotFile(File screenshotFile) throws IOException {
        Screenshot screenshot = Screenshot.loadFromFile(screenshotFile, 3840, 2160, null);
        BodyScannerResult result = new BodyScannerResult(screenshot);
        logger.trace("Scanning " + screenshot);

        // Apply transformations
        Region region = screenshot.getAsRegion();
        region.applyTransformation("BSTO", new KeepBodyScannerTextOnlyTransformation());
        region.applyTransformation("GRAY", new RgbToGrayTransformation());
        //        region.applyTransformation("THRESHHIGH", new ThresholdTransformation(224), "GRAY"); // Apply a very strong threshold for char locating
        //        GrayU8 thresholdedImage = (GrayU8) region.getImageData(Transformation.LAST);
        //        region.applyTransformation("THRESHMED", new ThresholdTransformation(128), "GRAY"); // Apply a moderate threshold for char matching
        region.applyTransformation("THRESH", new ThresholdTransformation(128));
        GrayU8 thresholdedImage = (GrayU8) region.getImageData(Transformation.LAST);
        region.applyTransformation("BLUR", new GaussianBlurTransformation(2, -1));

        // Find likely character locations and match them against the alphanum templates.
        // The alphanum templates are better to detect than small punctuation chars like dot, comma etc which are only a few pixels.
        // Also, the relevant words never start or end with a punctuation char. Therefore, we can find individual lines
        // of text from the matched alphanum chars.
        List<Rectangle> typicalCharacterSizeLocations = this.characterLocator.findLocationsOfTypicalCharacterSize(thresholdedImage);
        if (this.isDebugAlphanumTemplates()) {
            result.setAlphanumTemplatesDebugImage(this.debugTemplates(region, typicalCharacterSizeLocations, this.alphanumTemplates));
        }
        List<Match> alphanumMatches = new ArrayList<>(typicalCharacterSizeLocations.size());
        for (Rectangle r : typicalCharacterSizeLocations) {
            Region charRegion = region.getSubregion(r.x, r.y, r.width, r.height);
            Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, this.alphanumTemplates);
            if (bestMatch != null && bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_GUESSED) {
                alphanumMatches.add(bestMatch);
            } else {
                List<Match> nonOverlappingMatches = new TemplateMatcher().allNonOverlappingTemplates(charRegion, this.alphanumTemplates);
                nonOverlappingMatches = nonOverlappingMatches.stream().filter(m -> m.getErrorPerPixel() <= ERROR_PER_PIXEL_UNKNOWN).collect(Collectors.toList());
                if (nonOverlappingMatches.size() >= 2) {
                    alphanumMatches.addAll(nonOverlappingMatches);
                }
            }
        }

        // Now we should know quite certain where most alphanum chars are and can group them into text lines.
        // The text lines are more or less also just rectangles. Each such rectangle can then be scanned completely
        // in order to also find punctuation chars.
        List<TextLine> alphanumTextLines = TextBuilder.matchesToText(alphanumMatches);
        if (this.isDebugAlphanumTextLines()) {
            result.setAlphanumTextLinesDebugImage(this.debugTextLines(region, alphanumTextLines));
            for (TextLine tl : alphanumTextLines) {
                logger.debug(tl);
            }
        }
        List<Rectangle> locationsWithinTextLines = this.characterLocator.findLocationsWithinTextLines(thresholdedImage, alphanumTextLines);
        if (this.isDebugAllTemplates()) {
            result.setAllTemplatesDebugImage(this.debugTemplates(region, locationsWithinTextLines, this.allTemplates));
        }
        List<Match> allMatches = new ArrayList<>(locationsWithinTextLines.size());
        for (Rectangle r : locationsWithinTextLines) {
            Region charRegion = region.getSubregion(r.x, r.y, r.width, r.height);
            Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, this.allTemplates);
            if (bestMatch != null && bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_UNKNOWN) {
                allMatches.add(bestMatch);
            } else {
                List<Match> nonOverlappingMatches = new TemplateMatcher().allNonOverlappingTemplates(charRegion, this.alphanumTemplates);
                nonOverlappingMatches = nonOverlappingMatches.stream().filter(m -> m.getErrorPerPixel() <= ERROR_PER_PIXEL_UNKNOWN).collect(Collectors.toList());
                if (nonOverlappingMatches.size() >= 2) {
                    allMatches.addAll(nonOverlappingMatches);
                }
            }
        }

        // Build text lines again, this time from all matches
        List<TextLine> allTextLines = TextBuilder.matchesToText(allMatches);
        if (this.isDebugAllTextLines()) {
            result.setAllTextLinesDebugImage(this.debugTextLines(region, allTextLines));
            for (TextLine tl : allTextLines) {
                logger.debug(tl);
            }
        }

        // TODO
        List<TextLine> bodyNameLines = new ArrayList<>();
        List<Match> bodyInfoMatches = new ArrayList<>();
        for (TextLine tl : allTextLines) {
            if (tl.getxInScreenshot() > screenshot.getResizedWidth() * 0.333) {
                bodyNameLines.add(tl);
            } else {
                for (Word w : tl.getSortedWords()) {
                    bodyInfoMatches.addAll(w.getSortedMatches());
                }
            }
        }
        String filename = screenshotFile.getName();
        String systemName = MiscUtil.systemNameFromFilename(screenshotFile);
        ScannedBodyInfo sbi = BodyMatchesParser.fromScannedAndSortedMatches(filename, systemName, bodyNameLines, bodyInfoMatches);
        result.setScannedBodyInfo(sbi);

        if (this.isDebugFinal()) {
            result.setFinalDebugImage(this.debugFinal(region, allTextLines));
        }

        return result;
    }

    private BufferedImage debugFinal(Region region, List<TextLine> allTextLines) {
        BufferedImage gi = ConvertBufferedImage.convertTo_U8(region.getOriginalRGB(), null, true);
        BufferedImage bi = new BufferedImage(gi.getWidth(), gi.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = bi.createGraphics();
        g.drawImage(gi, 0, 0, null);
        g.setColor(Color.GREEN);
        g.setFont(new Font("Consolas", Font.PLAIN, 22));
        for (TextLine tl : allTextLines) {
            for (Word w : tl.getSortedWords()) {
                g.drawString(w.toText(true), w.getxInScreenshot(), w.getyInScreenshot());
            }
        }
        return bi;
    }

    private BufferedImage debugTemplates(Region region, List<Rectangle> locations, List<Template> templates) throws IOException {
        Random rand = new Random();
        BufferedImage gi = VisualizeImageData.grayMagnitude((ImageGray) region.getImageData(Transformation.LAST), null, -1);
        BufferedImage bi = new BufferedImage(gi.getWidth(), gi.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = bi.createGraphics();
        g.drawImage(gi, 0, 0, null);
        g.setFont(new Font("Consolas", Font.PLAIN, 22));
        for (Rectangle r : locations) {
            //g.setColor(Color.GRAY);
            g.setColor(new Color(64 + rand.nextInt(128), 64 + rand.nextInt(128), 64 + rand.nextInt(128)));
            g.drawRect(r.x, r.y, r.width, r.height);
        }

        // Match chars
        int nKnown = 0;
        int nGuessed = 0;
        int nUnknown = 0;
        int nCrap = 0;
        for (Rectangle r : locations) {
            Region charRegion = region.getSubregion(r.x, r.y, r.width, r.height);
            Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, templates);
            if (bestMatch == null) {
                // Most likely complete black
            } else if (bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_KNOWN) {
                nKnown++;
                Template.createNewFromRegion(charRegion, "KNOWN", bestMatch.getTemplate().getText());
                g.setFont(new Font("Consolas", Font.PLAIN, 22));
                g.setColor(Color.GREEN);
                g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
                //                AffineTransform transform = g.getTransform();
                //                g.rotate(Math.PI / 2, r.x, r.y + r.height);
                //                g.setFont(new Font("Arial", Font.PLAIN, 10));
                //                g.setColor(Color.WHITE);
                //                g.drawString(String.format(Locale.US, "%.3f", bestMatch.getErrorPerPixel()).substring(1), r.x, r.y + r.height);
                //                g.setTransform(transform);
            } else if (bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_GUESSED) {
                nGuessed++;
                Template.createNewFromRegion(charRegion, "GUESSED", bestMatch.getTemplate().getText());
                g.setFont(new Font("Consolas", Font.PLAIN, 22));
                g.setColor(Color.YELLOW);
                g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
                //                AffineTransform transform = g.getTransform();
                //                g.rotate(Math.PI / 2, r.x, r.y + r.height);
                //                g.setFont(new Font("Arial", Font.PLAIN, 10));
                //                g.setColor(Color.WHITE);
                //                g.drawString(String.format(Locale.US, "%.3f", bestMatch.getErrorPerPixel()).substring(1), r.x, r.y + r.height);
                //                g.setTransform(transform);
            } else if (bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_UNKNOWN) {
                nUnknown++;
                Template.createNewFromRegion(charRegion, "UNKNOWN", "UNKNOWN");
                g.setFont(new Font("Consolas", Font.PLAIN, 22));
                g.setColor(Color.RED);
                g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
                //                AffineTransform transform = g.getTransform();
                //                g.rotate(Math.PI / 2, r.x, r.y + r.height);
                //                g.setFont(new Font("Arial", Font.PLAIN, 10));
                //                g.setColor(Color.WHITE);
                //                g.drawString(String.format(Locale.US, "%.3f", bestMatch.getErrorPerPixel()).substring(1), r.x, r.y + r.height);
                //                g.setTransform(transform);
            } else {
                nCrap++;
                Template.createNewFromRegion(charRegion, "CRAP", "CRAP");
                g.setFont(new Font("Consolas", Font.PLAIN, 22));
                g.setColor(Color.RED);
                g.drawRect(r.x, r.y, r.width, r.height);
                //                AffineTransform transform = g.getTransform();
                //                g.rotate(Math.PI / 2, r.x, r.y + r.height);
                //                g.setFont(new Font("Arial", Font.PLAIN, 10));
                //                g.setColor(Color.WHITE);
                //                g.drawString(String.format(Locale.US, "%.3f", bestMatch.getErrorPerPixel()).substring(1), r.x, r.y + r.height);
                //                g.setTransform(transform);
            }

            if (bestMatch != null && bestMatch.getErrorPerPixel() > ERROR_PER_PIXEL_GUESSED) {
                List<Match> nonOverlappingMatches = new TemplateMatcher().allNonOverlappingTemplates(charRegion, templates);
                nonOverlappingMatches = nonOverlappingMatches.stream().filter(m -> m.getErrorPerPixel() <= ERROR_PER_PIXEL_UNKNOWN).collect(Collectors.toList());
                if (nonOverlappingMatches.size() >= 2) {
                    g.setFont(new Font("Consolas", Font.PLAIN, 22));
                    g.setColor(Color.MAGENTA);
                    for (Match m : nonOverlappingMatches) {
                        g.drawRect(m.getxInScreenshot(), m.getyInScreenshot(), m.getWidth(), m.getHeight());
                        g.drawString(m.getTemplate().getText(), m.getxInScreenshot(), m.getyInScreenshot());
                    }
                }
            }
        }
        logger.debug(region.getScreenshot().getFile().getName() + ": known=" + nKnown + "; guessed=" + nGuessed + "; unknown=" + nUnknown + "; crap=" + nCrap);

        // Return debug image
        return bi;
    }

    private BufferedImage debugTextLines(Region region, List<TextLine> textLines) {
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

    CharacterLocator getCharacterLocator() {
        return this.characterLocator;
    }

    List<Template> getAllTemplates() {
        return this.allTemplates;
    }

    List<Template> getAlphanumTemplates() {
        return this.alphanumTemplates;
    }

    public boolean isDebugAlphanumTemplates() {
        return this.debugAlphanumTemplates;
    }

    public void setDebugAlphanumTemplates(boolean debugAlphanumTemplates) {
        this.debugAlphanumTemplates = debugAlphanumTemplates;
    }

    public boolean isDebugAlphanumTextLines() {
        return this.debugAlphanumTextLines;
    }

    public void setDebugAlphanumTextLines(boolean debugAlphanumTextLines) {
        this.debugAlphanumTextLines = debugAlphanumTextLines;
    }

    public boolean isDebugAllTemplates() {
        return this.debugAllTemplates;
    }

    public void setDebugAllTemplates(boolean debugAllTemplates) {
        this.debugAllTemplates = debugAllTemplates;
    }

    public boolean isDebugAllTextLines() {
        return this.debugAllTextLines;
    }

    public void setDebugAllTextLines(boolean debugAllTextLines) {
        this.debugAllTextLines = debugAllTextLines;
    }

    public boolean isDebugFinal() {
        return this.debugFinal;
    }

    public void setDebugFinal(boolean debugFinal) {
        this.debugFinal = debugFinal;
    }

}
