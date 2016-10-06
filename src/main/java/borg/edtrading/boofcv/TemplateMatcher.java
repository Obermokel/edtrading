package borg.edtrading.boofcv;

import boofcv.abst.distort.FDistort;
import boofcv.alg.feature.detect.template.TemplateMatching;
import boofcv.alg.interpolate.TypeInterpolate;
import boofcv.factory.feature.detect.template.FactoryTemplateMatching;
import boofcv.factory.feature.detect.template.TemplateScoreType;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.io.image.UtilImageIO;
import boofcv.struct.feature.Match;
import boofcv.struct.image.GrayF32;
import borg.edtrading.Constants;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

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
                    t.setCroppedImage(cropCharToHeight(image));
                    result.add(t);
                }
            }
        }

        logger.debug("Loaded " + result.size() + " template(s) from " + type);

        return result;
    }

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

    public static TemplateMatch findBestTemplateMatch(BufferedImage unknownImage, List<Template> templates, int xWithinImage, int yWithinImage, String screenshotFilename) {
        try {
            GrayF32 grayUnknownImage = ConvertBufferedImage.convertFrom(unknownImage, (GrayF32) null);
            GrayF32 croppedUnknownImage = cropCharToHeight(grayUnknownImage);
            float croppedUnknownAR = (float) croppedUnknownImage.width / (float) croppedUnknownImage.height;

            final double pixels = croppedUnknownImage.width * croppedUnknownImage.height;
            final double maxErrorPerPixel = 750.0;
            double bestErrorPerPixel = maxErrorPerPixel;
            TemplateMatch bestMatch = null;
            for (Template template : templates) {
                float croppedTemplateAR = (float) template.getCroppedImage().width / (float) template.getCroppedImage().height;
                if (croppedTemplateAR <= 1.5 * croppedUnknownAR && croppedTemplateAR >= croppedUnknownAR / 1.5) {
                    GrayF32 scaledCroppedTemplateImage = new GrayF32(croppedUnknownImage.width, croppedUnknownImage.height);
                    new FDistort().input(template.getCroppedImage()).output(scaledCroppedTemplateImage).interp(TypeInterpolate.BICUBIC).scale().apply();
                    double error = 0.0;
                    for (int y = 0; y < croppedUnknownImage.height; y++) {
                        for (int x = 0; x < croppedUnknownImage.width; x++) {
                            float diff = croppedUnknownImage.unsafe_get(x, y) - scaledCroppedTemplateImage.unsafe_get(x, y);
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
                final double maxErrorPerPixelGuess = 9999.9; // We need a match for learning, regardless how bad it is
                double bestErrorPerPixelGuess = maxErrorPerPixelGuess;
                TemplateMatch bestGuess = null;
                for (Template template : templates) {
                    float croppedTemplateAR = (float) template.getCroppedImage().width / (float) template.getCroppedImage().height;
                    if (pixels <= 100 || (croppedTemplateAR <= 1.5 * croppedUnknownAR && croppedTemplateAR >= croppedUnknownAR / 1.5)) {
                        GrayF32 scaledCroppedTemplateImage = new GrayF32(croppedUnknownImage.width, croppedUnknownImage.height);
                        new FDistort().input(template.getCroppedImage()).output(scaledCroppedTemplateImage).interp(TypeInterpolate.BICUBIC).scale().apply();
                        double error = 0.0;
                        for (int y = 0; y < croppedUnknownImage.height; y++) {
                            for (int x = 0; x < croppedUnknownImage.width; x++) {
                                float diff = croppedUnknownImage.unsafe_get(x, y) - scaledCroppedTemplateImage.unsafe_get(x, y);
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
                if (!"verify.png".equals(screenshotFilename)) {
                    String folderName = bestGuess == null ? "UNKNOWN" : TemplateMatcher.textToFolder(bestGuess.getTemplate().getText());
                    String filenamePrefix = String.format(Locale.US, "%06.1f#%s", bestErrorPerPixelGuess, folderName);
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
            text = "";
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
        } else if ("".equals(text)) {
            folder = "_crap";
        }
        return folder;
    }

}
