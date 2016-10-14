package borg.edtrading.boofcv;

import boofcv.abst.distort.FDistort;
import boofcv.alg.feature.detect.template.TemplateMatching;
import boofcv.alg.interpolate.TypeInterpolate;
import boofcv.alg.misc.ImageMiscOps;
import boofcv.factory.feature.detect.template.FactoryTemplateMatching;
import boofcv.factory.feature.detect.template.TemplateScoreType;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.io.image.UtilImageIO;
import boofcv.struct.feature.Match;
import boofcv.struct.image.GrayF32;
import borg.edtrading.Constants;
import borg.edtrading.ocrOLD.CharacterFinder;
import borg.edtrading.ocrOLD.ScreenshotPreprocessor;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.RasterFormatException;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

/**
 * TemplateMatcher
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TemplateMatcher {

    static final Logger logger = LogManager.getLogger(TemplateMatcher.class);

    public static List<Template> loadTemplates(String type) {
        List<Template> result = new ArrayList<>();

        File baseDir = new File(Constants.TEMPLATES_DIR, type);
        if (baseDir.exists()) {
            File[] subDirs = baseDir.listFiles(new FileFilter() {
                @Override
                public boolean accept(File file) {
                    return file.isDirectory();
                }
            });
            for (File subDir : subDirs) {
                File[] pngFiles = subDir.listFiles(new FileFilter() {
                    @Override
                    public boolean accept(File file) {
                        return file.getName().toLowerCase().endsWith(".png") && !file.getName().toLowerCase().endsWith("_mask.png");
                    }
                });
                for (File pngFile : pngFiles) {
                    String folder = subDir.getName();
                    String text = folderToText(folder);
                    GrayF32 image = UtilImageIO.loadImage(pngFile.getAbsolutePath(), GrayF32.class);
                    GrayF32 mask = null;
                    File maskFile = new File(pngFile.getParentFile(), pngFile.getName().replace(".png", "_mask.png"));
                    if (maskFile.exists()) {
                        mask = UtilImageIO.loadImage(maskFile.getAbsolutePath(), GrayF32.class);
                    }
                    Template t = new Template(text, image, mask);
                    t.setCroppedImage(cropImage(image));
                    result.add(t);
                }
            }
        }

        logger.debug("Loaded " + result.size() + " template(s) from " + type);

        return result;
    }

    /**
     * @deprecated
     */
    @Deprecated
    public static List<TemplateMatch> findTemplateMatches(BufferedImage image, Template template, int maxMatches) {
        try {
            logger.trace("Searching max " + maxMatches + " match(es) of <" + template.getText() + ">");

            TemplateMatching<GrayF32> matcher = FactoryTemplateMatching.createMatcher(TemplateScoreType.SUM_DIFF_SQ, GrayF32.class);
            matcher.setTemplate(template.getImage(), template.getMask(), maxMatches);
            matcher.process(ConvertBufferedImage.convertFrom(image, (GrayF32) null));

            //int templatePixels = template.getImage().getWidth() * template.getImage().getHeight();
            //double maxScore = 1000.0 * templatePixels;

            List<TemplateMatch> result = new ArrayList<>(maxMatches);
            for (Match match : matcher.getResults().toList()) {
                //if (match.score < maxScore) {
                result.add(new TemplateMatch(template, match, null));
                //}
            }

            return result;
        } catch (Exception e) {
            logger.error("Failed to find max " + maxMatches + " matches for " + template + " in BufferedImage", e);
            return new ArrayList<>(0);
        }
    }

    public static TemplateMatch findBestMatch(BufferedImage image, GrayF32 imageF32, List<Template> templates) {
        return findBestMatch(image, imageF32, templates, Constants.MAX_ERROR_PER_PIXEL, null);
    }

    /**
     * @param image Original image
     * @param imageF32 Original image already converted to GrayF32 (for performance reasons). If <code>null</code> it will be converted by this method.
     * @param templates The templates to search in the image
     * @param maxErrorPerPixel Regarding the pixels in the template, not the image
     * @param maxCroppedSizeDifference If not <code>null</code> the image will also be cropped, and if its cropped size is too different from the template, the template will be skipped immediately
     * @return The best match, or <code>null</code> if none was good enough
     */
    public static TemplateMatch findBestMatch(BufferedImage image, GrayF32 imageF32, List<Template> templates, double maxErrorPerPixel, Float maxCroppedSizeDifference) {
        if (imageF32 == null) {
            imageF32 = ConvertBufferedImage.convertFrom(image, (GrayF32) null);
        }
        Integer minCroppedTemplateWidth = null;
        Integer maxCroppedTemplateWidth = null;
        Integer minCroppedTemplateHeight = null;
        Integer maxCroppedTemplateHeight = null;
        if (maxCroppedSizeDifference != null) {
            GrayF32 croppedImageF32 = cropImage(imageF32).getGrayF32();
            minCroppedTemplateWidth = Math.round(croppedImageF32.width / maxCroppedSizeDifference);
            maxCroppedTemplateWidth = Math.round(croppedImageF32.width * maxCroppedSizeDifference);
            minCroppedTemplateHeight = Math.round(croppedImageF32.height / maxCroppedSizeDifference);
            maxCroppedTemplateHeight = Math.round(croppedImageF32.height * maxCroppedSizeDifference);
        }
        double bestErrorPerPixel = maxErrorPerPixel;
        TemplateMatch bestMatch = null;
        for (Template t : templates) {
            // Always use the cropped template. If not already available crop it now.
            CroppedImage croppedTemplate = t.getCroppedImage() != null ? t.getCroppedImage() : cropImage(t.getImage());
            GrayF32 templateF32 = croppedTemplate.getGrayF32();
            // Fast skip: Cropped sizes too different?
            if (maxCroppedSizeDifference != null) {
                if (templateF32.width < minCroppedTemplateWidth || templateF32.width > maxCroppedTemplateWidth || templateF32.height < minCroppedTemplateHeight || templateF32.height > maxCroppedTemplateHeight) {
                    continue; // Fast skip to next template
                }
            }
            final double pixels = templateF32.width * templateF32.height;
            for (int yInImage = 0; yInImage < imageF32.height - templateF32.height; yInImage++) {
                for (int xInImage = 0; xInImage < imageF32.width - templateF32.width; xInImage++) {
                    double error = 0.0;
                    for (int yInTemplate = 0; yInTemplate < templateF32.height; yInTemplate++) {
                        for (int xInTemplate = 0; xInTemplate < templateF32.width; xInTemplate++) {
                            float diff = imageF32.unsafe_get(xInImage + xInTemplate, yInImage + yInTemplate) - templateF32.unsafe_get(xInTemplate, yInTemplate);
                            error += (diff * diff);
                        }
                    }
                    double errorPerPixel = error / pixels;
                    if (errorPerPixel < bestErrorPerPixel) {
                        bestErrorPerPixel = errorPerPixel;
                        bestMatch = new TemplateMatch(t, new Match(xInImage - croppedTemplate.getxInOriginal(), yInImage - croppedTemplate.getyInOriginal(), -1 * error), image);
                    }
                }
            }
        }
        return bestMatch;
    }

    public static List<TemplateMatch> findAllNonOverlappingMatches(BufferedImage image, GrayF32 imageF32, List<Template> templates) {
        // Find all
        List<TemplateMatch> allMatches = new ArrayList<>();
        for (Template t : templates) {
            TemplateMatch bestMatch = findBestMatch(image, imageF32, Arrays.asList(t));
            if (bestMatch != null) {
                allMatches.add(bestMatch);
            }
        }
        if (allMatches.isEmpty()) {
            return allMatches;
        } else {
            // Sort by error/pixel
            Collections.sort(allMatches, new Comparator<TemplateMatch>() {
                @Override
                public int compare(TemplateMatch m1, TemplateMatch m2) {
                    return new Double(m1.getErrorPerPixel()).compareTo(new Double(m2.getErrorPerPixel()));
                }
            });
            // Select all non-overlapping
            List<TemplateMatch> nonOverlappingMatches = new ArrayList<>();
            nonOverlappingMatches.add(allMatches.remove(0)); // Add the best one
            for (TemplateMatch m : allMatches) {
                if (!m.overlapsWithAny(nonOverlappingMatches)) {
                    nonOverlappingMatches.add(m);
                }
            }
            // Finished
            return nonOverlappingMatches;
        }
    }

    /**
     * Sorted by x-position in the image
     */
    public static List<TemplateMatch> findSortedMatches(BufferedImage image, GrayF32 imageF32, List<Template> templates, List<String> combinations) {
        // Find the best single match, assuming the image already almost represents exactly one of the templates.
        // That is, the image does not contain multiple templates, and also has a quite correct size/aspect ratio.
        TemplateMatch bestSingleMatch = findBestMatch(image, imageF32, templates, Constants.MAX_ERROR_PER_PIXEL, 1.25f);
        if (bestSingleMatch != null) {
            return Arrays.asList(bestSingleMatch);
        } else {
            // Test all allowed combinations
            String concat = StringUtils.join(combinations);
            List<Template> templatesForCombinations = templates.stream().filter(t -> concat.contains(t.getText())).collect(Collectors.toList());
            List<TemplateMatch> combinationMatches = findAllNonOverlappingMatches(image, imageF32, templatesForCombinations);
            Collections.sort(combinationMatches, new Comparator<TemplateMatch>() {
                @Override
                public int compare(TemplateMatch m1, TemplateMatch m2) {
                    return new Integer(m1.getMatch().x).compareTo(new Integer(m2.getMatch().x));
                }
            });
            String joinedText = StringUtils.join(combinationMatches.stream().map(m -> m.getTemplate().getText()).collect(Collectors.toList()));
            if (combinations.contains(joinedText)) {
                return combinationMatches;
            } else {
                // Guess using a higher allowed error/pixel and size difference
                TemplateMatch bestGuess = findBestMatch(image, imageF32, templates, 999 * Constants.MAX_ERROR_PER_PIXEL, 1.5f);
                if (bestGuess != null) {
                    return Arrays.asList(bestGuess);
                } else {
                    return Collections.emptyList();
                }
            }
        }
    }

    public static List<TemplateMatch> findSortedMatchesInScreenshot(BufferedImage screenshotImage, List<Template> templates, List<String> combinations, String screenshotFilename) throws IOException {
        Constants.UNKNOWN_DIR.mkdirs();

        List<Rectangle> characterLocations = CharacterFinder.findCharacterLocations(screenshotImage, screenshotImage, false);
        BufferedImage blurredImage = ScreenshotPreprocessor.gaussian(screenshotImage, 2);

        List<TemplateMatch> sortedScreenshotMatches = new ArrayList<>(characterLocations.size());
        for (Rectangle r : characterLocations) {
            try {
                BufferedImage charImage = blurredImage.getSubimage(r.x, r.y, r.width, r.height);
                GrayF32 imageF32 = ConvertBufferedImage.convertFrom(charImage, (GrayF32) null);
                List<TemplateMatch> sortedCharMatches = findSortedMatches(charImage, imageF32, templates, combinations);
                if (sortedCharMatches.isEmpty()) {
                    // Write to unknown dir
                    ImageIO.write(charImage, "PNG", new File(Constants.UNKNOWN_DIR, "UNKNOWN#" + r.x + "#" + r.y + "#" + screenshotFilename));
                } else {
                    // Adjust x and y, then add to result
                    List<TemplateMatch> adjustedMatches = new ArrayList<>(sortedCharMatches.size());
                    for (TemplateMatch m : sortedCharMatches) {
                        adjustedMatches.add(new TemplateMatch(m.getTemplate(), new Match(m.getMatch().x + r.x, m.getMatch().y + r.y, m.getMatch().score), m.getMatchedImage()));
                    }
                    sortedScreenshotMatches.addAll(adjustedMatches);
                }
            } catch (RasterFormatException e) {
                logger.warn("Character location outside of raster: " + r + " in " + screenshotFilename);
            }
        }

        // Sort by x, then by y
        Collections.sort(sortedScreenshotMatches, new Comparator<TemplateMatch>() {
            @Override
            public int compare(TemplateMatch m1, TemplateMatch m2) {
                return new Integer(m1.getMatch().x).compareTo(new Integer(m2.getMatch().x));
            }
        });
        Collections.sort(sortedScreenshotMatches, new Comparator<TemplateMatch>() {
            @Override
            public int compare(TemplateMatch m1, TemplateMatch m2) {
                return new Integer(m1.getMatch().y).compareTo(new Integer(m2.getMatch().y));
            }
        });
        return sortedScreenshotMatches;
    }

    public static List<TemplateMatch> findMultiTemplateMatches(BufferedImage unknownImage, List<Template> templates, List<String> combinations, int xWithinImage, int yWithinImage, String screenshotFilename) {
        for (String combination : combinations) {
            GrayF32 unknownF32 = ConvertBufferedImage.convertFrom(unknownImage, (GrayF32) null);
            List<TemplateMatch> result = new ArrayList<>();
            for (int charIndex = 0; charIndex < combination.length(); charIndex++) {
                String c = Character.toString(combination.charAt(charIndex));
                List<Template> templatesForChar = templates.stream().filter(t -> t.getText().equals(c)).collect(Collectors.toList());
                //logger.info("Searching for <" + c + "> in combination <" + combination + "> using " + templatesForChar.size() + " template(s)");
                final double maxErrorPerPixel = 1500.0;
                double bestErrorPerPixel = maxErrorPerPixel;
                TemplateMatch bestMatch = null;
                Rectangle bestMatchRect = null;
                for (Template t : templatesForChar) {
                    int scaleToWidth = Math.round((float) t.getImage().width * ((float) unknownF32.height / (float) t.getImage().height));
                    GrayF32 templateF32Scaled = new GrayF32(scaleToWidth, unknownF32.height);
                    new FDistort().input(t.getImage()).output(templateF32Scaled).interp(TypeInterpolate.BICUBIC).scale().apply();
                    final double pixels = templateF32Scaled.width * templateF32Scaled.height;
                    for (int xOffset = 0; xOffset < unknownF32.width - templateF32Scaled.width; xOffset++) {
                        double error = 0.0;
                        for (int y = 0; y < templateF32Scaled.height; y++) {
                            for (int x = 0; x < templateF32Scaled.width; x++) {
                                float diff = unknownF32.unsafe_get(x + xOffset, y) - templateF32Scaled.unsafe_get(x, y);
                                error += (diff * diff);
                            }
                        }
                        double errorPerPixel = error / pixels;
                        if (errorPerPixel < bestErrorPerPixel) {
                            bestErrorPerPixel = errorPerPixel;
                            bestMatch = new TemplateMatch(t, new Match(xWithinImage + xOffset, yWithinImage, -1 * error), unknownImage);
                            bestMatchRect = new Rectangle(xOffset, 0, templateF32Scaled.width, templateF32Scaled.height);
                        }
                    }
                }
                if (bestMatch == null) {
                    //logger.info("Not found");
                    break; // Test the next combination
                } else {
                    //logger.info("Found with " + bestErrorPerPixel + " errpp");
                    ImageMiscOps.fillRectangle(unknownF32, 0.0f, bestMatchRect.x, bestMatchRect.y, bestMatchRect.width, bestMatchRect.height); // Overwrite with black to avoid matching again
                    result.add(bestMatch); // Test the next char in the current combination
                }
            }
            if (result.size() == combination.length()) {
                //logger.info("Found combination: " + combination);
                return result; // This is it!
            }
        }

        return null; // None matched
    }

    public static TemplateMatch findBestTemplateMatch(BufferedImage unknownImage, List<Template> templates, int xWithinImage, int yWithinImage, String screenshotFilename) {
        try {
            GrayF32 originalUnknownImage = ConvertBufferedImage.convertFrom(unknownImage, (GrayF32) null);
            GrayF32 croppedUnknownImage = cropCharToHeight(originalUnknownImage);
            final boolean doNotUseCropped = (croppedUnknownImage.width * croppedUnknownImage.height) <= 100;
            GrayF32 unknownF32 = doNotUseCropped ? originalUnknownImage : croppedUnknownImage;
            final float unknownAR = (float) unknownF32.width / (float) unknownF32.height;
            final double pixels = unknownF32.width * unknownF32.height;

            final double maxErrorPerPixel = 750.0;
            double bestErrorPerPixel = maxErrorPerPixel;
            TemplateMatch bestMatch = null;
            for (Template template : templates) {
                GrayF32 templateF32 = doNotUseCropped ? template.getImage() : template.getCroppedImage().getGrayF32();
                float templateAR = (float) templateF32.width / (float) templateF32.height;
                if (templateAR <= 1.25 * unknownAR && templateAR >= unknownAR / 1.25) {
                    GrayF32 templateF32Scaled = new GrayF32(unknownF32.width, unknownF32.height);
                    new FDistort().input(templateF32).output(templateF32Scaled).interp(TypeInterpolate.BICUBIC).scale().apply();
                    double error = 0.0;
                    for (int y = 0; y < unknownF32.height; y++) {
                        for (int x = 0; x < unknownF32.width; x++) {
                            float diff = unknownF32.unsafe_get(x, y) - templateF32Scaled.unsafe_get(x, y);
                            error += (diff * diff);
                        }
                    }
                    double errorPerPixel = error / pixels;
                    if (errorPerPixel < bestErrorPerPixel) {
                        bestErrorPerPixel = errorPerPixel;
                        bestMatch = new TemplateMatch(template, new Match(xWithinImage, yWithinImage, -1 * error), unknownImage);
                    }
                }
            }
            if (bestMatch == null) {
                final double maxErrorPerPixelGuess = 99999.9; // We need a match for learning, regardless how bad it is
                double bestErrorPerPixelGuess = maxErrorPerPixelGuess;
                TemplateMatch bestGuess = null;
                for (Template template : templates) {
                    GrayF32 templateF32 = doNotUseCropped ? template.getImage() : template.getCroppedImage().getGrayF32();
                    float templateAR = (float) templateF32.width / (float) templateF32.height;
                    if (templateAR <= 1.5 * unknownAR && templateAR >= unknownAR / 1.5) {
                        GrayF32 templateF32Scaled = new GrayF32(unknownF32.width, unknownF32.height);
                        new FDistort().input(templateF32).output(templateF32Scaled).interp(TypeInterpolate.BICUBIC).scale().apply();
                        double error = 0.0;
                        for (int y = 0; y < unknownF32.height; y++) {
                            for (int x = 0; x < unknownF32.width; x++) {
                                float diff = unknownF32.unsafe_get(x, y) - templateF32Scaled.unsafe_get(x, y);
                                error += (diff * diff);
                            }
                        }
                        double errorPerPixel = error / pixels;
                        if (errorPerPixel < bestErrorPerPixelGuess) {
                            bestErrorPerPixelGuess = errorPerPixel;
                            bestGuess = new TemplateMatch(template, new Match(xWithinImage, yWithinImage, -1 * error), unknownImage);
                        }
                    }
                }
                if (!"verify.png".equals(screenshotFilename) && bestErrorPerPixelGuess >= 2 * maxErrorPerPixel) {
                    String folderName = bestGuess == null ? "UNKNOWN" : TemplateMatcher.textToFolder(bestGuess.getTemplate().getText());
                    String filenamePrefix = String.format(Locale.US, "%07.1f#%s", bestErrorPerPixelGuess, folderName);
                    String filenameSuffix = xWithinImage + "#" + yWithinImage + "#" + screenshotFilename;
                    Constants.UNKNOWN_DIR.mkdirs();
                    ImageIO.write(unknownImage, "PNG", new File(Constants.UNKNOWN_DIR, filenamePrefix + "#" + filenameSuffix));
                }
                bestMatch = bestGuess;
            }
            return bestMatch;
        } catch (Exception e) {
            logger.error("Failed to find best match in BufferedImage", e);
            return null;
        }
    }

    private static GrayF32 cropCharToHeight(GrayF32 originalCharImage) {
        int lastBlackLineFromTop = 0;
        boolean foundTopBoundary = false;
        for (int y = 0; y < originalCharImage.height && !foundTopBoundary; y++) {
            for (int x = 0; x < originalCharImage.width && !foundTopBoundary; x++) {
                if (originalCharImage.unsafe_get(x, y) != 0f) {
                    foundTopBoundary = true;
                }
            }
            lastBlackLineFromTop = y;
        }

        int lastBlackLineFromBottom = originalCharImage.height - 1;
        boolean foundBottomBoundary = false;
        for (int y = originalCharImage.height - 1; y >= 0 && !foundBottomBoundary; y--) {
            for (int x = 0; x < originalCharImage.width && !foundBottomBoundary; x++) {
                if (originalCharImage.unsafe_get(x, y) != 0f) {
                    foundBottomBoundary = true;
                }
            }
            lastBlackLineFromBottom = y;
        }

        if (foundTopBoundary && foundBottomBoundary && lastBlackLineFromTop < lastBlackLineFromBottom) {
            return originalCharImage.subimage(0, lastBlackLineFromTop, originalCharImage.width, lastBlackLineFromBottom + 1);
        } else {
            return originalCharImage;
        }
    }

    private static CroppedImage cropImage(GrayF32 originalImage) {
        int firstNonBlackLineFromTop = 0;
        boolean foundTopBoundary = false;
        for (int y = 0; y < originalImage.height && !foundTopBoundary; y++) {
            for (int x = 0; x < originalImage.width && !foundTopBoundary; x++) {
                if (originalImage.unsafe_get(x, y) != 0f) {
                    foundTopBoundary = true;
                }
            }
            firstNonBlackLineFromTop = y;
        }

        int firstNonBlackLineFromBottom = originalImage.height - 1;
        boolean foundBottomBoundary = false;
        for (int y = originalImage.height - 1; y >= 0 && !foundBottomBoundary; y--) {
            for (int x = 0; x < originalImage.width && !foundBottomBoundary; x++) {
                if (originalImage.unsafe_get(x, y) != 0f) {
                    foundBottomBoundary = true;
                }
            }
            firstNonBlackLineFromBottom = y;
        }

        int firstNonBlackLineFromLeft = 0;
        boolean foundLeftBoundary = false;
        for (int x = 0; x < originalImage.width && !foundLeftBoundary; x++) {
            for (int y = 0; y < originalImage.height && !foundLeftBoundary; y++) {
                if (originalImage.unsafe_get(x, y) != 0f) {
                    foundLeftBoundary = true;
                }
            }
            firstNonBlackLineFromLeft = x;
        }

        int firstNonBlackLineFromRight = originalImage.width - 1;
        boolean foundRightBoundary = false;
        for (int x = originalImage.width - 1; x >= 0 && !foundRightBoundary; x--) {
            for (int y = 0; y < originalImage.height && !foundRightBoundary; y++) {
                if (originalImage.unsafe_get(x, y) != 0f) {
                    foundRightBoundary = true;
                }
            }
            firstNonBlackLineFromRight = x;
        }

        boolean foundAllBoundaries = foundTopBoundary && foundBottomBoundary && foundLeftBoundary && foundRightBoundary;

        if (foundAllBoundaries && firstNonBlackLineFromTop < firstNonBlackLineFromBottom && firstNonBlackLineFromLeft < firstNonBlackLineFromRight) {
            int x0Incl = firstNonBlackLineFromLeft;
            int y0Incl = firstNonBlackLineFromTop;
            int x1Excl = firstNonBlackLineFromRight + 1;
            int y1Excl = firstNonBlackLineFromBottom + 1;

            return new CroppedImage(originalImage.subimage(x0Incl, y0Incl, x1Excl, y1Excl), firstNonBlackLineFromLeft, firstNonBlackLineFromTop);
        } else {
            return new CroppedImage(originalImage, 0, 0);
        }
    }

    public static String folderToText(String folder) {
        String text = folder;
        if (folder.endsWith("_")) {
            text = text.substring(0, text.length() - 1).toLowerCase();
        } else if ("_punkt".equals(folder)) {
            text = ".";
        } else if ("_komma".equals(folder)) {
            text = ",";
        } else if ("_apostroph".equals(folder)) {
            text = "'";
        } else if ("_mikro".equals(folder)) {
            text = "µ";
        } else if ("_prozent".equals(folder)) {
            text = "%";
        } else if ("_strich".equals(folder)) {
            text = "-";
        } else if ("_plus".equals(folder)) {
            text = "+";
        } else if ("_doppelpunkt".equals(folder)) {
            text = ":";
        } else if ("_klammer_auf".equals(folder)) {
            text = "(";
        } else if ("_klammer_zu".equals(folder)) {
            text = ")";
        } else if ("_grad".equals(folder)) {
            text = "°";
        } else if ("_crap".equals(folder)) {
            text = "▪";
        }
        return text;
    }

    public static String textToFolder(String text) {
        String folder = text;
        if (text.matches("[a-z]+")) {
            folder = folder + "_";
        } else if (".".equals(text)) {
            folder = "_punkt";
        } else if (",".equals(text)) {
            folder = "_komma";
        } else if ("'".equals(text)) {
            folder = "_apostroph";
        } else if ("µ".equals(text)) {
            folder = "_mikro";
        } else if ("%".equals(text)) {
            folder = "_prozent";
        } else if ("-".equals(text)) {
            folder = "_strich";
        } else if ("+".equals(text)) {
            folder = "_plus";
        } else if (":".equals(text)) {
            folder = "_doppelpunkt";
        } else if ("(".equals(text)) {
            folder = "_klammer_auf";
        } else if (")".equals(text)) {
            folder = "_klammer_zu";
        } else if ("°".equals(text)) {
            folder = "_grad";
        } else if ("▪".equals(text)) {
            folder = "_crap";
        }
        return folder;
    }

    public static class CroppedImage {

        private final GrayF32 grayF32;
        private final int xInOriginal;
        private final int yInOriginal;

        public CroppedImage(GrayF32 grayF32, int xInOriginal, int yInOriginal) {
            this.grayF32 = grayF32;
            this.xInOriginal = xInOriginal;
            this.yInOriginal = yInOriginal;
        }

        public GrayF32 getGrayF32() {
            return this.grayF32;
        }

        public int getxInOriginal() {
            return this.xInOriginal;
        }

        public int getyInOriginal() {
            return this.yInOriginal;
        }

    }

}
