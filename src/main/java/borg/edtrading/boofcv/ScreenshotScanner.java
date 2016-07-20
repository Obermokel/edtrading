package borg.edtrading.boofcv;

import boofcv.alg.color.ColorHsv;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.io.image.UtilImageIO;
import boofcv.struct.image.ImageFloat32;
import boofcv.struct.image.MultiSpectral;
import georegression.metric.UtilAngle;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * ScreenshotScanner
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScreenshotScanner {

    static final Logger logger = LogManager.getLogger(ScreenshotScanner.class);

    public static Map<Integer, Map<String, String>> scanScreenshot(File screenshotFile) {
        // Load the screenshot and all templates
        BufferedImage screenshotImage = UtilImageIO.loadImage(screenshotFile.getAbsolutePath());
        List<Template> templates = TemplateMatcher.loadTemplates();

        // Simplify the screenshot so that orange text is better to recognize
        BufferedImage orangeTextImage = keepOrangeTextOnly(screenshotImage);

        // Crop the image to the actual commodities market
        BufferedImage croppedImage = cropToCommoditiesMarket(orangeTextImage, templates);

        // Find cols and rows
        Map<String, Integer> columns = findColumns(croppedImage, templates);
        List<Integer> rows = findRows(croppedImage, templates, columns);

        // Remove hairlines which are problematic for pattern matching
        BufferedImage noHairlineImage = removeHairlines(croppedImage, rows, columns);

        // Find template matches and group them per row/col
        Map<Integer, Map<String, List<TemplateMatch>>> groupedMatches = findAndGroupMatches(noHairlineImage, templates, rows, columns);

        // Extract the actual texts
        Map<Integer, Map<String, String>> texts = extractTexts(groupedMatches);

        // Finished
        return texts;
    }

    public static BufferedImage keepOrangeTextOnly(BufferedImage image) {
        return keepTextOnly(image, (float) Math.toRadians(25), 0.91f, 252f);
    }

    private static BufferedImage keepTextOnly(BufferedImage image, float hue, float saturation, float value) {
        logger.debug("Simplifying image");

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

    public static BufferedImage cropToCommoditiesMarket(BufferedImage image, List<Template> templates) {
        logger.debug("Cropping to commodities market");

        // We need to find...
        // ...<WAREN> for the upper left corner
        // ...<DURCHSCHNITT> for the right border (can be color inverted)
        // ...<VERLASSEN> for the lower border (can be color inverted)
        List<Template> templatesWaren = lookupTemplatesWithText(templates, "WAREN");
        List<Template> templatesDurchschnitt = lookupTemplatesWithText(templates, "DURCHSCHNITT");
        List<Template> templatesVerlassen = lookupTemplatesWithText(templates, "VERLASSEN");

        // === FULL IMAGE SCAN ===
        //        TemplateMatch bestMatchWaren = findBestTemplateMatch(image, templatesWaren);
        //        TemplateMatch bestMatchDurchschnitt = findBestTemplateMatch(image, templatesDurchschnitt);
        //        TemplateMatch bestMatchVerlassen = findBestTemplateMatch(image, templatesVerlassen);
        //
        //        int x = bestMatchWaren.getMatch().x;
        //        int y = bestMatchWaren.getMatch().y;
        //        int w = (bestMatchDurchschnitt.getMatch().x + bestMatchDurchschnitt.getTemplate().getImage().width) - x;
        //        int h = (bestMatchVerlassen.getMatch().y + bestMatchVerlassen.getTemplate().getImage().height) - y;

        // === QUARTER IMAGE SCAN ===
        //        int halfWidth = image.getWidth() / 2;
        //        int halfHeight = image.getHeight() / 2;
        //        TemplateMatch bestMatchWaren = findBestTemplateMatch(image.getSubimage(0, 0, halfWidth, halfHeight), templatesWaren);
        //        TemplateMatch bestMatchDurchschnitt = findBestTemplateMatch(image.getSubimage(halfWidth, 0, halfWidth, halfHeight), templatesDurchschnitt);
        //        TemplateMatch bestMatchVerlassen = findBestTemplateMatch(image.getSubimage(0, halfHeight, halfWidth, halfHeight), templatesVerlassen);
        //
        //        int x = bestMatchWaren.getMatch().x;
        //        int y = bestMatchWaren.getMatch().y;
        //        int w = (bestMatchDurchschnitt.getMatch().x + bestMatchDurchschnitt.getTemplate().getImage().width + halfWidth) - x;
        //        int h = (bestMatchVerlassen.getMatch().y + bestMatchVerlassen.getTemplate().getImage().height + halfHeight) - y;

        // === 3440x1440 IMAGE SCAN ===
        TemplateMatch bestMatchWaren = findBestTemplateMatch(image.getSubimage(500, 250, 200, 100), templatesWaren);
        TemplateMatch bestMatchDurchschnitt = findBestTemplateMatch(image.getSubimage(1850, 250, 350, 100), templatesDurchschnitt);
        TemplateMatch bestMatchVerlassen = findBestTemplateMatch(image.getSubimage(500, 1250, 280, 180), templatesVerlassen);

        int x = bestMatchWaren.getMatch().x + 500;
        int y = bestMatchWaren.getMatch().y + 250;
        int w = (bestMatchDurchschnitt.getMatch().x + bestMatchDurchschnitt.getTemplate().getImage().width + 1850) - x;
        int h = (bestMatchVerlassen.getMatch().y + bestMatchVerlassen.getTemplate().getImage().height + 1250) - y;

        return image.getSubimage(x, y, w, h);
    }

    /**
     * @return Map of x coords where the columns are
     */
    public static Map<String, Integer> findColumns(BufferedImage image, List<Template> templates) {
        logger.debug("Searching columns");

        // Because the image should be cropped we only need to search within the uppermost 50 px
        BufferedImage headline = image.getSubimage(0, 0, image.getWidth(), 50);

        TemplateMatch bestMatchWaren = findBestTemplateMatch(headline, lookupTemplatesWithText(templates, "WAREN"));
        TemplateMatch bestMatchVerkauf = findBestTemplateMatch(headline, lookupTemplatesWithText(templates, "VERKAUF"));
        TemplateMatch bestMatchEinkauf = findBestTemplateMatch(headline, lookupTemplatesWithText(templates, "EINKAUF"));
        TemplateMatch bestMatchFracht = findBestTemplateMatch(headline, lookupTemplatesWithText(templates, "FRACHT"));
        TemplateMatch bestMatchNachfrage = findBestTemplateMatch(headline, lookupTemplatesWithText(templates, "NACHFRAGE"));
        TemplateMatch bestMatchAufLager = findBestTemplateMatch(headline, lookupTemplatesWithText(templates, "AUFLAGER"));
        TemplateMatch bestMatchDurchschnitt = findBestTemplateMatch(headline, lookupTemplatesWithText(templates, "DURCHSCHNITT"));

        Map<String, Integer> result = new LinkedHashMap<>();
        result.put("WAREN", bestMatchWaren.getMatch().x);
        result.put("VERKAUF", bestMatchVerkauf.getMatch().x);
        result.put("EINKAUF", bestMatchEinkauf.getMatch().x);
        result.put("FRACHT", bestMatchFracht.getMatch().x);
        result.put("NACHFRAGE", bestMatchNachfrage.getMatch().x);
        result.put("AUFLAGER", bestMatchAufLager.getMatch().x);
        result.put("DURCHSCHNITT", bestMatchDurchschnitt.getMatch().x);
        return result;
    }

    public static List<Integer> findRows(BufferedImage image, List<Template> templates, Map<String, Integer> columns) {
        logger.debug("Searching rows");

        final int maxRowsExpected = 20;

        // Only search within the relevant cols
        int fromX = columns.get("NACHFRAGE");
        int toX = columns.get("DURCHSCHNITT");
        BufferedImage subimage = image.getSubimage(fromX, 0, toX - fromX, image.getHeight());

        // Find all possible high/med/low
        List<TemplateMatch> matches = new ArrayList<>();
        for (Template template : lookupTemplatesWithText(templates, "HOCH")) {
            matches.addAll(TemplateMatcher.findTemplateMatches(subimage, template, maxRowsExpected));
        }
        for (Template template : lookupTemplatesWithText(templates, "MITTEL")) {
            matches.addAll(TemplateMatcher.findTemplateMatches(subimage, template, maxRowsExpected));
        }
        for (Template template : lookupTemplatesWithText(templates, "NIEDRIG")) {
            matches.addAll(TemplateMatcher.findTemplateMatches(subimage, template, maxRowsExpected));
        }

        // Sort by score
        Collections.sort(matches, new Comparator<TemplateMatch>() {
            @Override
            public int compare(TemplateMatch m1, TemplateMatch m2) {
                return new Integer(m1.getMatchQuality()).compareTo(new Integer(m2.getMatchQuality()));
            }
        });

        // Select all non-overlapping
        List<TemplateMatch> nonOverlapping = new ArrayList<>();
        for (TemplateMatch match : matches) {
            if (!match.overlapsWithAny(nonOverlapping)) {
                nonOverlapping.add(match);
            }
        }

        // Use the best ones
        List<Integer> result = new ArrayList<>();
        for (TemplateMatch match : nonOverlapping) {
            if (match.getMatchQuality() <= 25) {
                logger.debug("Using <" + match.getTemplate().getText() + "> at y=" + match.getMatch().y + " with Q=" + match.getMatchQuality());
                result.add(match.getMatch().y);
            } else {
                logger.debug("Skipping <" + match.getTemplate().getText() + "> at y=" + match.getMatch().y + " with Q=" + match.getMatchQuality());
            }
        }

        // Sort by row y and return
        Collections.sort(result);
        return result;
    }

    public static BufferedImage removeHairlines(BufferedImage image, List<Integer> rows, Map<String, Integer> columns) {
        BufferedImage noHairlineImage = new BufferedImage(image.getWidth(), image.getHeight(), image.getType());
        Graphics2D g = noHairlineImage.createGraphics();
        g.drawImage(image, 0, 0, image.getWidth(), image.getHeight(), null);
        g.setColor(Color.BLACK);
        for (Integer x : columns.values()) {
            g.fillRect(x - 4, 0, 8, image.getHeight());
        }
        int lastYStart = 0;
        for (Integer currentYStart : rows) {
            int lastYEnd = lastYStart + 50;
            int height = currentYStart - lastYEnd;
            g.fillRect(0, lastYEnd, image.getWidth(), height);
            lastYStart = currentYStart;
        }
        g.fillRect(0, lastYStart + 50, image.getWidth(), image.getHeight() - (lastYStart + 50));
        g.dispose();
        return noHairlineImage;
    }

    private static List<Template> lookupTemplatesWithText(List<Template> templates, String text) {
        List<Template> result = new ArrayList<>();

        for (Template template : templates) {
            if (template.getText().equals(text)) {
                result.add(template);
            }
        }

        return result;
    }

    /**
     * @return First key is row number (starting at 0), second key is column name, value is list of matches for that combination
     */
    public static Map<Integer, Map<String, List<TemplateMatch>>> findAndGroupMatches(BufferedImage image, List<Template> templates, List<Integer> rows, Map<String, Integer> columns) {
        Map<Integer, Map<String, List<TemplateMatch>>> result = new LinkedHashMap<>();

        int rowNum = 0;
        for (Integer rowPx : rows) {
            logger.debug("Find and group matches for row " + rowNum);

            // Extract a smaller row image
            BufferedImage rowImage = image.getSubimage(0, rowPx, image.getWidth(), 50);

            // Collect all matches
            List<TemplateMatch> matches = new ArrayList<>();
            for (Template template : templates) {
                if (!template.isSpecial()) {
                    matches.addAll(TemplateMatcher.findTemplateMatches(rowImage, template, 13)); // max 13 matches should be enough for E, 0 and other frequent chars
                }
            }

            // Sort by score
            Collections.sort(matches, new Comparator<TemplateMatch>() {
                @Override
                public int compare(TemplateMatch m1, TemplateMatch m2) {
                    return new Integer(m1.getMatchQuality()).compareTo(new Integer(m2.getMatchQuality()));
                }
            });

            // Select all non-overlapping
            List<TemplateMatch> nonOverlapping = new ArrayList<>();
            for (TemplateMatch match : matches) {
                if (!match.overlapsWithAny(nonOverlapping)) {
                    nonOverlapping.add(match);
                }
            }

            // Sort by x
            Collections.sort(nonOverlapping, new Comparator<TemplateMatch>() {
                @Override
                public int compare(TemplateMatch m1, TemplateMatch m2) {
                    return new Integer(m1.getMatch().x).compareTo(new Integer(m2.getMatch().x));
                }
            });

            // Group into columns
            Map<String, List<TemplateMatch>> matchesByColumn = new LinkedHashMap<>();
            for (TemplateMatch match : nonOverlapping) {
                String column = lookupColumnOfMatch(match, columns);
                List<TemplateMatch> columnMatches = matchesByColumn.get(column);
                if (columnMatches == null) {
                    columnMatches = new ArrayList<>();
                    matchesByColumn.put(column, columnMatches);
                }
                columnMatches.add(match);
            }

            // Add to result
            result.put(rowNum++, matchesByColumn);
        }

        return result;
    }

    private static String lookupColumnOfMatch(TemplateMatch match, Map<String, Integer> columns) {
        final int x = match.getMatch().x;

        String maxColumnName = null;
        for (String columnName : columns.keySet()) {
            int columnX = columns.get(columnName);
            if (x >= columnX) {
                maxColumnName = columnName;
            }
        }

        return maxColumnName;
    }

    private static TemplateMatch findBestTemplateMatch(BufferedImage image, List<Template> templates) {
        TemplateMatch bestMatch = null;

        for (Template template : templates) {
            List<TemplateMatch> matches = TemplateMatcher.findTemplateMatches(image, template, 1);
            if (!matches.isEmpty()) {
                TemplateMatch match = matches.get(0);
                if (bestMatch == null || bestMatch.getMatchQuality() > match.getMatchQuality()) {
                    bestMatch = match;
                }
            }
        }

        return bestMatch;
    }

    public static Map<Integer, Map<String, String>> extractTexts(Map<Integer, Map<String, List<TemplateMatch>>> groupedMatches) {
        Map<Integer, Map<String, String>> result = new LinkedHashMap<>();

        for (Integer rowNum : groupedMatches.keySet()) {
            Map<String, List<TemplateMatch>> rowMatches = groupedMatches.get(rowNum);
            Map<String, String> rowTexts = new LinkedHashMap<>();

            for (String columnName : rowMatches.keySet()) {
                List<TemplateMatch> columnMatches = rowMatches.get(columnName);
                StringBuilder columnText = new StringBuilder();

                for (TemplateMatch match : columnMatches) {
                    columnText.append(match.getTemplate().getText());
                }

                rowTexts.put(columnName, columnText.toString());
            }

            result.put(rowNum, rowTexts);
        }

        return result;
    }

}
