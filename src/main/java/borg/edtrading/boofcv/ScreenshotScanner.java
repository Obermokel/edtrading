package borg.edtrading.boofcv;

import java.io.File;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import boofcv.io.image.UtilImageIO;
import boofcv.struct.image.ImageFloat32;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * ScreenshotScanner
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScreenshotScanner {

    static final Logger logger = LogManager.getLogger(ScreenshotScanner.class);

    public static void scanScreenshot(File screenshotFile) {
        // Load screenshot
        ImageFloat32 screenshotImage = UtilImageIO.loadImage(screenshotFile.getAbsolutePath(), ImageFloat32.class);

        // Find matches
        List<TemplateMatch> allMatches = TemplateMatcher.findAllTemplateMatches(screenshotImage);

        // Sort
        sortTemplateMatches(allMatches);

        // Print
        printTemplateMatches(allMatches);
    }

    private static void sortTemplateMatches(List<TemplateMatch> templateMatches) {
        Collections.sort(templateMatches, new Comparator<TemplateMatch>() {
            @Override
            public int compare(TemplateMatch m1, TemplateMatch m2) {
                int dy = m1.getMatch().y - m2.getMatch().y;
                if (dy < -10) {
                    return -1; // m1 is clearly above m2
                } else if (dy > 10) {
                    return 1; // m1 is clearly below m2
                } else {
                    return new Integer(m1.getMatch().x).compareTo(new Integer(m2.getMatch().x)); // Compare x position
                }
            }
        });
    }

    private static void printTemplateMatches(List<TemplateMatch> templateMatches) {
        for (TemplateMatch templateMatch : templateMatches) {
            System.out.print(templateMatch.getTemplate().getText());
        }
        System.out.println();
    }

}
