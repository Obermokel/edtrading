package borg.edtrading.ocr;

import boofcv.gui.image.VisualizeImageData;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageGray;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.simple.GaussianBlurTransformation;
import borg.edtrading.imagetransformation.simple.KeepBodyScannerTextOnlyTransformation;
import borg.edtrading.imagetransformation.simple.RgbToGrayTransformation;
import borg.edtrading.imagetransformation.simple.ThresholdTransformation;
import borg.edtrading.ocr.screenshots.Region;
import borg.edtrading.ocr.templatematching.Match;
import borg.edtrading.ocr.templatematching.Template;
import borg.edtrading.ocr.templatematching.TemplateMatcher;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

/**
 * OcrExecutor
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class OcrExecutor {

    static final Logger logger = LogManager.getLogger(OcrExecutor.class);

    /**
     * If error/pixel is less or equal than this value we should be 99.9% safe that it has been detected correctly.
     */
    public static final float ERROR_PER_PIXEL_KNOWN = 0.015f;
    /**
     * If error/pixel is less or equal than this value the detection quality should be good enough for only slight
     * errors which can be corrected with levenshtein and known texts.
     */
    public static final float ERROR_PER_PIXEL_GUESSED = 0.035f;
    /**
     * If error/pixel is less or equal than this value we assume that the pixels represent an unknown char. If
     * error/pixel is higher it is likely to be crap.
     */
    public static final float ERROR_PER_PIXEL_UNKNOWN = 0.075f;

    public OcrResult executeOcr(OcrTask ocrTask) {
        OcrResult ocrResult = new OcrResult(ocrTask);

        // Apply transformations
        Region region = ocrTask.getScreenshotRegion();
        region.applyTransformation("BSTO", new KeepBodyScannerTextOnlyTransformation());
        region.applyTransformation("GRAY", new RgbToGrayTransformation());
        region.applyTransformation("THRESH", new ThresholdTransformation(128));
        GrayU8 thresholdedImage = (GrayU8) region.getImageData(Transformation.LAST);
        region.applyTransformation("BLUR", new GaussianBlurTransformation(2, -1));
        GrayF32 blurredImage = (GrayF32) region.getImageData(Transformation.LAST);
        if (ocrTask.isDebugThresholdImage()) {
            ocrResult.setThresholdDebugImage(VisualizeImageData.grayMagnitude(thresholdedImage, null, -1));
        }
        if (ocrTask.isDebugBlurredImage()) {
            ocrResult.setBlurredDebugImage(VisualizeImageData.grayMagnitude(blurredImage, null, -1));
        }

        // Find likely character locations and match them against the alphanum templates.
        // The alphanum templates are better to detect than small punctuation chars like dot, comma etc which are only a few pixels.
        // Also, the relevant words never start or end with a punctuation char. Therefore, we can find individual lines
        // of text from the matched alphanum chars.
        List<Template> alphanumTemplates = ocrTask.getTemplates().stream().filter(t -> t.getText().matches("[A-Za-z0-9]")).collect(Collectors.toList());
        List<Rectangle> typicalCharacterSizeLocations = ocrTask.getCharacterLocator().findLocationsOfTypicalCharacterSize(thresholdedImage);
        if (ocrTask.isDebugAlphanumTemplates()) {
            try {
                ocrResult.setAlphanumTemplatesDebugImage(this.debugTemplates(region, typicalCharacterSizeLocations, alphanumTemplates));
            } catch (IOException ex) {
                logger.error("Failed to create debug image", ex);
            }
        }
        List<Match> alphanumMatches = new ArrayList<>(typicalCharacterSizeLocations.size());
        for (Rectangle r : typicalCharacterSizeLocations) {
            Region charRegion = region.getSubregion(r.x, r.y, r.width, r.height);
            Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, alphanumTemplates);
            if (bestMatch != null && bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_GUESSED) {
                // The rectangle seems to contain a single char
                alphanumMatches.add(bestMatch);
            } else {
                // The rectangle might contain a multiple chars -> find all non-overlapping
                List<Match> nonOverlappingMatches = new TemplateMatcher().allNonOverlappingTemplates(charRegion, alphanumTemplates);
                nonOverlappingMatches = nonOverlappingMatches.stream().filter(m -> m.getErrorPerPixel() <= ERROR_PER_PIXEL_GUESSED).collect(Collectors.toList());
                if (nonOverlappingMatches.size() >= 2) {
                    alphanumMatches.addAll(nonOverlappingMatches);
                }
            }
        }

        // Now we should know quite certain where most alphanum chars are and can group them into text lines.
        // The text lines are more or less also just rectangles. Each such rectangle can then be scanned completely
        // in order to also find punctuation chars.
        List<TextLine> alphanumTextLines = TextBuilder.matchesToText(alphanumMatches);
        if (ocrTask.isDebugAlphanumTextLines()) {
            ocrResult.setAlphanumTextLinesDebugImage(this.debugTextLines(region, alphanumTextLines));
        }
        List<Rectangle> locationsWithinTextLines = ocrTask.getCharacterLocator().findLocationsWithinTextLines(thresholdedImage, alphanumTextLines);
        if (ocrTask.isDebugAllTemplates()) {
            try {
                ocrResult.setAllTemplatesDebugImage(this.debugTemplates(region, locationsWithinTextLines, ocrTask.getTemplates()));
            } catch (IOException ex) {
                logger.error("Failed to create debug image", ex);
            }
        }
        List<Match> allMatches = new ArrayList<>(locationsWithinTextLines.size());
        for (Rectangle r : locationsWithinTextLines) {
            Region charRegion = region.getSubregion(r.x, r.y, r.width, r.height);
            Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, ocrTask.getTemplates());
            if (bestMatch != null && bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_UNKNOWN) {
                allMatches.add(bestMatch);
            } else {
                List<Match> nonOverlappingMatches = new TemplateMatcher().allNonOverlappingTemplates(charRegion, alphanumTemplates); // TODO Only alphanum or also punctuation?
                nonOverlappingMatches = nonOverlappingMatches.stream().filter(m -> m.getErrorPerPixel() <= ERROR_PER_PIXEL_UNKNOWN).collect(Collectors.toList());
                if (nonOverlappingMatches.size() >= 2) {
                    allMatches.addAll(nonOverlappingMatches);
                }
            }
        }

        // Build text lines again, this time from all matches
        List<TextLine> allTextLines = TextBuilder.matchesToText(allMatches);
        if (ocrTask.isDebugAllTextLines()) {
            ocrResult.setAllTextLinesDebugImage(this.debugTextLines(region, allTextLines));
        }

        // Set the result and return
        ocrResult.setTextLines(allTextLines);

        return ocrResult;
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
                //Template.createNewFromRegion(charRegion, "KNOWN", bestMatch.getTemplate().getText());
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

}
