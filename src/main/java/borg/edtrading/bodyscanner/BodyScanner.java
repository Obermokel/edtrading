package borg.edtrading.bodyscanner;

import boofcv.gui.image.VisualizeImageData;
import boofcv.struct.image.GrayU8;
import borg.edtrading.data.ScannedBodyInfo;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.simple.GaussianBlurTransformation;
import borg.edtrading.imagetransformation.simple.KeepBodyScannerTextOnlyTransformation;
import borg.edtrading.imagetransformation.simple.RgbToGrayTransformation;
import borg.edtrading.imagetransformation.simple.ThresholdTransformation;
import borg.edtrading.ocr.CharacterLocator;
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
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
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
    private static final float ERROR_PER_PIXEL_KNOWN = 0.015f;
    /**
     * If error/pixel is less or equal than this value the detection quality should be good enough for only slight
     * errors which can be corrected with levenshtein and known texts.
     */
    private static final float ERROR_PER_PIXEL_GUESSED = 0.05f;
    /**
     * If error/pixel is less or equal than this value we assume that the pixels represent an unknown char. If
     * error/pixel is higher it is likely to be crap.
     */
    private static final float ERROR_PER_PIXEL_UNKNOWN = 0.25f;

    static final Logger logger = LogManager.getLogger(BodyScanner.class);

    private final CharacterLocator characterLocator;
    private final List<Template> allTemplates;
    private final List<Template> alphanumTemplates;

    private boolean debugAlphanumTemplates = false;

    public BodyScanner() throws IOException {
        this.characterLocator = new CharacterLocator(2, 40, 16, 40, 1);
        this.allTemplates = Template.fromFolder("BodyScanner");
        this.alphanumTemplates = Template.fromFolder("BodyScanner").stream().filter(t -> t.getText().matches("\\w")).collect(Collectors.toList());
    }

    public BodyScannerResult scanScreenshotFile(File screenshotFile) throws IOException {
        Screenshot screenshot = Screenshot.loadFromFile(screenshotFile, 3840, 2160, null);
        BodyScannerResult result = new BodyScannerResult(screenshot);
        logger.trace("Scanning " + screenshot);

        // Apply transformations
        Region region = screenshot.getAsRegion();
        region.applyTransformation(new KeepBodyScannerTextOnlyTransformation());
        region.applyTransformation(new RgbToGrayTransformation());
        region.applyTransformation(new ThresholdTransformation(128));
        GrayU8 thresholdedImage = (GrayU8) region.getImageData(Transformation.LAST);
        region.applyTransformation(new GaussianBlurTransformation(2, -1));

        // Find likely character locations and match them against the alphanum templates
        List<Rectangle> typicalCharacterSizeLocations = this.characterLocator.findLocationsOfTypicalCharacterSize(thresholdedImage);
        if (this.isDebugAlphanumTemplates()) {
            result.setAlphanumTemplatesDebugImage(this.debugAlphanumTemplates(region, typicalCharacterSizeLocations, this.alphanumTemplates));
        }
        List<Match> matches = new ArrayList<>(typicalCharacterSizeLocations.size());
        for (Rectangle r : typicalCharacterSizeLocations) {
            Region charRegion = region.getSubregion(r.x, r.y, r.width, r.height);
            Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, this.alphanumTemplates);
            if (bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_GUESSED) {
                matches.add(bestMatch);
            }
        }
        TextLine.matchesToTextLines(matches);

        ScannedBodyInfo scannedBodyInfo = null;

        return result;
    }

    private BufferedImage debugAlphanumTemplates(Region region, List<Rectangle> typicalCharacterSizeLocations, List<Template> alphanumTemplates) throws IOException {
        BufferedImage gi = VisualizeImageData.grayMagnitude((GrayU8) region.getImageData(Transformation.LAST), null, -1);
        BufferedImage bi = new BufferedImage(gi.getWidth(), gi.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = bi.createGraphics();
        g.drawImage(gi, 0, 0, null);
        g.setFont(new Font("Consolas", Font.PLAIN, 22));
        g.setColor(Color.GRAY);
        for (Rectangle r : typicalCharacterSizeLocations) {
            g.drawRect(r.x, r.y, r.width, r.height);
        }

        // Match chars
        int nKnown = 0;
        int nGuessed = 0;
        int nUnknown = 0;
        int nCrap = 0;
        for (Rectangle r : typicalCharacterSizeLocations) {
            Region charRegion = region.getSubregion(r.x, r.y, r.width, r.height);
            Match bestMatch = new TemplateMatcher().bestMatchingTemplate(charRegion, alphanumTemplates);
            if (bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_KNOWN) {
                nKnown++;
                Template.createNewFromRegion(charRegion, "KNOWN", bestMatch.getTemplate().getText());
                g.setColor(Color.GREEN);
                g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
            } else if (bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_GUESSED) {
                nGuessed++;
                Template.createNewFromRegion(charRegion, "GUESSED", bestMatch.getTemplate().getText());
                g.setColor(Color.YELLOW);
                g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
            } else if (bestMatch.getErrorPerPixel() <= ERROR_PER_PIXEL_UNKNOWN) {
                nUnknown++;
                Template.createNewFromRegion(charRegion, "UNKNOWN", "UNKNOWN");
                g.setColor(Color.RED);
                g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
            } else {
                nCrap++;
                g.setColor(Color.RED);
                g.drawRect(r.x, r.y, r.width, r.height);
            }
        }
        logger.debug(region.getScreenshot().getFile().getName() + ": known=" + nKnown + "; guessed=" + nGuessed + "; unknown=" + nUnknown + "; crap=" + nCrap);

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

}
