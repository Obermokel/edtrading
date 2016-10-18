package borg.edtrading.imagetransformation.simple;

import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.struct.image.GrayS16;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageBase;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.TransformationException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * CannyEdgeTransformation
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CannyEdgeTransformation implements Transformation {

    static final Logger logger = LogManager.getLogger(CannyEdgeTransformation.class);

    private final int blurRadius;
    private final float thresholdLow;
    private final float thresholdHigh;

    public CannyEdgeTransformation() {
        this(2, 0.1f, 0.3f);
    }

    public CannyEdgeTransformation(int blurRadius, float thresholdLow, float thresholdHigh) {
        this.blurRadius = blurRadius;
        this.thresholdLow = thresholdLow;
        this.thresholdHigh = thresholdHigh;
    }

    @Override
    public String getName() {
        return this.getClass().getSimpleName();
    }

    @Override
    public ImageBase<?> transform(ImageBase<?> inputImage) throws TransformationException {
        try {
            GrayU8 inputGrayU8 = (GrayU8) inputImage;
            GrayU8 outputGrayU8 = inputGrayU8.createSameShape();
            CannyEdge<GrayU8, GrayS16> canny = FactoryEdgeDetectors.canny(this.getBlurRadius(), true, true, GrayU8.class, GrayS16.class);
            canny.process(inputGrayU8, this.getThresholdLow(), this.getThresholdHigh(), outputGrayU8);
            return outputGrayU8;
        } catch (ClassCastException e) {
            throw new TransformationException("Input image must be a GrayU8", e);
        }
    }

    public int getBlurRadius() {
        return this.blurRadius;
    }

    public float getThresholdLow() {
        return this.thresholdLow;
    }

    public float getThresholdHigh() {
        return this.thresholdHigh;
    }

}
