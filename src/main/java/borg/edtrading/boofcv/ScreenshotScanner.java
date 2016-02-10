package borg.edtrading.boofcv;

import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;

import boofcv.alg.color.ColorHsv;
import boofcv.alg.feature.detect.template.TemplateMatching;
import boofcv.factory.feature.detect.template.FactoryTemplateMatching;
import boofcv.factory.feature.detect.template.TemplateScoreType;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.io.image.UtilImageIO;
import boofcv.struct.feature.Match;
import boofcv.struct.image.ImageFloat32;
import boofcv.struct.image.MultiSpectral;
import georegression.metric.UtilAngle;
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
        // Load screenshot and templates
        ImageFloat32 screenshotImage = UtilImageIO.loadImage(screenshotFile.getAbsolutePath(), ImageFloat32.class);
        List<Template> templates = TemplateLoader.loadTemplates();

        // Collect matches
        List<TemplateMatch> allMatches = new ArrayList<>();
        for (Template template : templates) {
            allMatches.addAll(findTemplateInScreenshot(template, screenshotImage));
        }

        // Sort
        sortTemplateMatches(allMatches);

        // Print
        printTemplateMatches(allMatches);
    }

    /**
     * Only useful for the commodity screen
     */
    public static BufferedImage crop3440x1440(BufferedImage image) {
        return image.getSubimage(540, 0, 1620, 1440);
    }

    public static BufferedImage keepOrangeTextOnly(BufferedImage image) {
        return keepTextOnly(image, (float) Math.toRadians(25), 0.91f, 252f);
    }

    private static BufferedImage keepTextOnly(BufferedImage image, float hue, float saturation, float value) {
        MultiSpectral<ImageFloat32> input = ConvertBufferedImage.convertFromMulti(image, null, true, ImageFloat32.class);
        MultiSpectral<ImageFloat32> hsv = input.createSameShape();

        // Convert into HSV
        ColorHsv.rgbToHsv_F32(input, hsv);

        // Euclidean distance squared threshold for deciding which pixels are members of the selected set
        float maxDist2 = 0.5f * 0.5f;

        // Extract hue and saturation bands which are independent of intensity
        ImageFloat32 H = hsv.getBand(0);
        ImageFloat32 S = hsv.getBand(1);
        ImageFloat32 V = hsv.getBand(2);

        // step through each pixel and mark how close it is to the selected color
        BufferedImage output = new BufferedImage(input.width, input.height, BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < hsv.height; y++) {
            for (int x = 0; x < hsv.width; x++) {
                // Hue is an angle in radians, so simple subtraction doesn't work
                float dh = UtilAngle.dist(H.unsafe_get(x, y), hue) / (float) Math.PI;
                float ds = (S.unsafe_get(x, y) - saturation);
                float dv = (V.unsafe_get(x, y) - value) / 255f;

                // this distance measure is a bit naive, but good enough for to demonstrate the concept
                float dist2 = dh * dh + ds * ds + dv * dv;
                if (dist2 <= maxDist2) {
                    output.setRGB(x, y, image.getRGB(x, y));
                }
            }
        }

        return output;
    }

    private static List<TemplateMatch> findTemplateInScreenshot(Template template, ImageFloat32 screenshotImage) {
        TemplateMatching<ImageFloat32> matcher = FactoryTemplateMatching.createMatcher(TemplateScoreType.SUM_DIFF_SQ, ImageFloat32.class);

        // Find the points which match the template the best
        matcher.setTemplate(template.getImage(), null, 20);
        matcher.process(screenshotImage);

        int templatePixels = template.getImage().getWidth() * template.getImage().getHeight();
        double maxScore = 735.0 * templatePixels;

        List<TemplateMatch> result = new ArrayList<>();
        ListIterator<Match> it = matcher.getResults().toList().listIterator();
        while (it.hasNext()) {
            Match match = it.next();
            if (match.score < maxScore) {
                result.add(new TemplateMatch(template, match));
            }
        }

        return result;
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
