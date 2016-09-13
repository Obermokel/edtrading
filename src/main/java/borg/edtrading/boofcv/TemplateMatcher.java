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

/**
 * TemplateMatcher
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TemplateMatcher {

    static final Logger logger = LogManager.getLogger(TemplateMatcher.class);

    public static List<Template> loadTemplates(String type) {
        logger.debug("Loading templates");

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
                    String text = subDir.getName();
                    boolean special = false;
                    if (subDir.getName().endsWith("_")) {
                        text = text.substring(0, text.length() - 1).toLowerCase();
                    } else if ("_punkt".equals(subDir.getName())) {
                        text = ".";
                    } else if ("_komma".equals(subDir.getName())) {
                        text = ",";
                    } else if ("_prozent".equals(subDir.getName())) {
                        text = "%";
                    } else if ("_strich".equals(subDir.getName())) {
                        text = "-";
                    } else if ("_doppelpunkt".equals(subDir.getName())) {
                        text = ":";
                    } else if ("_klammer_auf".equals(subDir.getName())) {
                        text = "(";
                    } else if ("_klammer_zu".equals(subDir.getName())) {
                        text = ")";
                    } else if ("_grad".equals(subDir.getName())) {
                        text = "°";
                    } else if ("_hoch".equals(subDir.getName())) {
                        text = "^3";
                    } else if ("_mittel".equals(subDir.getName())) {
                        text = "^2";
                    } else if ("_niedrig".equals(subDir.getName())) {
                        text = "^1";
                    } else if ("_space".equals(subDir.getName())) {
                        text = "";
                        continue; // FIXME
                    } else if ("__border".equals(subDir.getName())) {
                        text = pngFile.getName().replace(".png", "").replaceAll("\\d", "");
                        special = true;
                    }
                    GrayF32 image = UtilImageIO.loadImage(pngFile.getAbsolutePath(), GrayF32.class);
                    GrayF32 mask = null;
                    File maskFile = new File(pngFile.getParentFile(), pngFile.getName().replace(".png", "_mask.png"));
                    if (maskFile.exists()) {
                        mask = UtilImageIO.loadImage(maskFile.getAbsolutePath(), GrayF32.class);
                    }
                    result.add(new Template(text, image, mask, special));
                }
            }
        }

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
                result.add(new TemplateMatch(template, match));
                //}
            }

            return result;
        } catch (Exception e) {
            logger.error("Failed to find max " + maxMatches + " matches for " + template + " in BufferedImage", e);
            return new ArrayList<>(0);
        }
    }

    public static TemplateMatch findBestTemplateMatch(BufferedImage image, List<Template> templates, int xWithinImage, int yWithinImage) {
        try {
            GrayF32 grayImage = ConvertBufferedImage.convertFrom(image, (GrayF32) null);
            float imageAR = (float) image.getWidth() / (float) image.getHeight();

            final double pixels = image.getWidth() * image.getHeight();
            final double maxErrorPerPixel = 2000;
            double bestErrorPerPixel = maxErrorPerPixel;
            TemplateMatch bestMatch = null;
            for (Template template : templates) {
                float templateAR = (float) template.getImage().width / (float) template.getImage().height;
                if (templateAR <= 1.5 * imageAR && templateAR >= imageAR / 1.5) {
                    GrayF32 scaledTemplate = new GrayF32(grayImage.width, grayImage.height);
                    new FDistort().input(template.getImage()).output(scaledTemplate).interp(TypeInterpolate.BICUBIC).scale().apply();
                    double error = 0.0;
                    for (int y = 0; y < grayImage.height; y++) {
                        for (int x = 0; x < grayImage.width; x++) {
                            float diff = grayImage.unsafe_get(x, y) - scaledTemplate.unsafe_get(x, y);
                            error += (diff * diff);
                        }
                    }
                    double errorPerPixel = error / pixels;
                    if (errorPerPixel < bestErrorPerPixel) {
                        bestErrorPerPixel = errorPerPixel;
                        bestMatch = new TemplateMatch(template, new Match(xWithinImage, yWithinImage, -1 * errorPerPixel));
                    }
                }
            }
            //logger.debug(bestErrorPerPixel);
            return bestMatch;
        } catch (Exception e) {
            logger.error("Failed to find best match in BufferedImage", e);
            return null;
        }
    }

}
