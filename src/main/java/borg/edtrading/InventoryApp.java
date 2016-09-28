package borg.edtrading;

import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import borg.edtrading.ocr.CharacterFinder;
import borg.edtrading.ocr.ScreenshotPreprocessor;
import borg.edtrading.util.MatchSorter;
import borg.edtrading.util.MatchSorter.MatchGroup;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.RasterFormatException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * InventoryApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryApp {

    static final Logger logger = LogManager.getLogger(InventoryApp.class);

    static List<MatchGroup> scanWords(BufferedImage cannySuitableImage, List<Template> templates, String screenshotFilename) throws IOException {
        BufferedImage thresholdedWhiteCharsOnBlackBackgroundImage = ScreenshotPreprocessor.cannyEdge(cannySuitableImage);
        BufferedImage blurredImage = ScreenshotPreprocessor.gaussian(thresholdedWhiteCharsOnBlackBackgroundImage, 2);
        List<Rectangle> characterLocations = CharacterFinder.findCharacterLocations(cannySuitableImage, thresholdedWhiteCharsOnBlackBackgroundImage, false);

        List<TemplateMatch> matches = new ArrayList<>(characterLocations.size());
        for (Rectangle r : characterLocations) {
            try {
                BufferedImage charImage = blurredImage.getSubimage(r.x, r.y, r.width, r.height);
                TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(charImage, templates, r.x, r.y, screenshotFilename);
                if (bestMatch != null) {
                    matches.add(bestMatch);
                }
            } catch (RasterFormatException e) {
                // Ignore
            }
        }

        List<MatchGroup> matchGroups = MatchSorter.sortMatches(matches);
        //return matchGroups.stream().map(mg -> mg.getText()).collect(Collectors.toList());
        return matchGroups;
    }

}
