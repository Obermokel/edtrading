package borg.edtrading.templatematching;

import boofcv.struct.image.GrayF32;
import borg.edtrading.ocr.Region;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;

/**
 * TemplateMatcher
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TemplateMatcher {

    static final Logger logger = LogManager.getLogger(TemplateMatcher.class);

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
            GrayF32 regionPixels = (GrayF32) region.getTransformed(t.getTransformation());
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
                bestMatch = new Match(region, t, 0, 0, error, errorPerPixel);
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
        if (template.getWidth() > region.getWidth() || template.getHeight() > region.getHeight()) {
            throw new IllegalArgumentException("Template " + template + " is larger than " + region);
        } else {
            GrayF32 regionPixels = (GrayF32) region.getTransformed(template.getTransformation());
            float bestError = 999999999.9f;
            Match bestMatch = null;
            for (int yInRegion = 0; yInRegion < (region.getHeight() - template.getHeight()); yInRegion++) {
                for (int xInRegion = 0; xInRegion < (region.getWidth() - template.getWidth()); xInRegion++) {
                    float error = 0.0f;
                    for (int yInTemplate = 0; yInTemplate < template.getHeight() && error < bestError; yInTemplate++) {
                        for (int xInTemplate = 0; xInTemplate < template.getWidth() && error < bestError; xInTemplate++) {
                            float diff = regionPixels.unsafe_get(xInRegion + xInTemplate, yInRegion + yInTemplate) - template.getPixels().unsafe_get(xInTemplate, yInTemplate);
                            error += (diff * diff);
                        }
                    }
                    if (error < bestError) {
                        // Use the template size for error/pixel calculation because we are using the unscaled template
                        float errorPerPixel = error / (template.getWidth() * template.getHeight());
                        bestError = error;
                        bestMatch = new Match(region, template, xInRegion, yInRegion, error, errorPerPixel);
                    }
                }
            }
            return bestMatch;
        }
    }

}
