package borg.edtrading.templatematching;

import boofcv.struct.image.GrayF32;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.screenshots.Region;
import borg.edtrading.util.ImageUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * TemplateMatcher
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TemplateMatcher {

    static final Logger logger = LogManager.getLogger(TemplateMatcher.class);

    public List<Match> allNonOverlappingTemplates(Region region, List<Template> templates) {
        List<Match> allMatches = new ArrayList<>();
        for (Template t : templates) {
            if ((t.getWidth() - 2) <= region.getWidth() && (t.getHeight() - 2) <= region.getHeight()) {
                Match m = bestMatchingLocation(region, t, 1);
                if (m != null) {
                    allMatches.add(m);
                }
            }
        }
        Collections.sort(allMatches, new Comparator<Match>() {
            @Override
            public int compare(Match m1, Match m2) {
                return new Double(m1.getErrorPerPixel()).compareTo(new Double(m2.getErrorPerPixel()));
            }
        });
        List<Match> nonOverlappingMatches = new ArrayList<>();
        for (Match m : allMatches) {
            if (!m.overlapsWithAny(nonOverlappingMatches, -1)) {
                nonOverlappingMatches.add(m);
            }
        }
        return nonOverlappingMatches;
    }

    /**
     * Scales each template to the size of the screenshot region and finds the best matching one
     *
     * @return The best matching template, never <code>null</code>
     */
    public Match bestMatchingTemplate(Region region, List<Template> templates) {
        float bestError = 999999999.9f;
        Match bestMatch = null;
        for (Template t : templates) {
            GrayF32 scaledTemplatePixels = t.scalePixelsToSize(region.getWidth(), region.getHeight());
            GrayF32 regionPixels = ImageUtil.normalize((GrayF32) region.getImageData(Transformation.LAST));
            float error = 0.0f;
            for (int y = 0; y < region.getHeight() && error < bestError; y++) {
                for (int x = 0; x < region.getWidth() && error < bestError; x++) {
                    float diff = regionPixels.unsafe_get(x, y) - scaledTemplatePixels.unsafe_get(x, y);
                    error += (diff * diff);
                }
            }
            if (error < bestError) {
                // Use the region size for error/pixel calculation because we are using the scaled template
                float errorPerPixel = error / (region.getWidth() * region.getHeight());
                bestError = error;
                bestMatch = new Match(region, t, 0, 0, region.getWidth(), region.getHeight(), error, errorPerPixel);
            }
        }
        return bestMatch;
    }

    /**
     * Scans the screenshot region to find the best location of the (unscaled) template
     *
     * @return The best matching location, never <code>null</code>
     * @throws IllegalArgumentException If the template is larger than the screenshot region
     */
    public Match bestMatchingLocation(Region region, Template template) throws IllegalArgumentException {
        return this.bestMatchingLocation(region, template, 0);
    }

    /**
     * Scans the screenshot region to find the best location of the (unscaled) template
     *
     * @return The best matching location, never <code>null</code>
     * @throws IllegalArgumentException If the template is larger than the screenshot region
     */
    public Match bestMatchingLocation(Region region, Template template, int cropTemplate) throws IllegalArgumentException {
        if ((template.getWidth() - 2 * cropTemplate) > region.getWidth() || (template.getHeight() - 2 * cropTemplate) > region.getHeight()) {
            throw new IllegalArgumentException("Template " + template + " is larger than " + region);
        } else {
            GrayF32 regionPixels = ImageUtil.normalize((GrayF32) region.getImageData(Transformation.LAST));
            GrayF32 templatePixels = cropTemplate <= 0 ? template.getPixels() : template.getPixels().subimage(cropTemplate, cropTemplate, template.getWidth() - 2 * cropTemplate, template.getHeight() - 2 * cropTemplate);
            float bestError = 999999999.9f;
            Match bestMatch = null;
            for (int yInRegion = 0; yInRegion < (region.getHeight() - templatePixels.getHeight()); yInRegion++) {
                for (int xInRegion = 0; xInRegion < (region.getWidth() - templatePixels.getWidth()); xInRegion++) {
                    float error = 0.0f;
                    for (int yInTemplate = 0; yInTemplate < templatePixels.getHeight() && error < bestError; yInTemplate++) {
                        for (int xInTemplate = 0; xInTemplate < templatePixels.getWidth() && error < bestError; xInTemplate++) {
                            float diff = regionPixels.unsafe_get(xInRegion + xInTemplate, yInRegion + yInTemplate) - templatePixels.unsafe_get(xInTemplate, yInTemplate);
                            error += (diff * diff);
                        }
                    }
                    if (error < bestError) {
                        // Use the template size for error/pixel calculation because we are using the unscaled template
                        float errorPerPixel = error / (templatePixels.getWidth() * templatePixels.getHeight());
                        bestError = error;
                        bestMatch = new Match(region, template, xInRegion, yInRegion, templatePixels.width, templatePixels.height, error, errorPerPixel);
                    }
                }
            }
            return bestMatch;
        }
    }

}
