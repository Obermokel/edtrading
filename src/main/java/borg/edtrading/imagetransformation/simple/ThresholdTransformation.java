package borg.edtrading.imagetransformation.simple;

import boofcv.alg.filter.binary.GThresholdImageOps;
import boofcv.struct.image.ImageBase;
import boofcv.struct.image.ImageGray;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.TransformationException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * <p>
 * Input Image must be any <code>ImageGray</code>.
 * </p>
 * <p>
 * Output Image will be <code>GrayU8</code> with values only being 0 or 1.
 * </p>
 * <p>
 * Threshold must be set according to the input image, for example 128 for a GrayU8 input
 * with values from 0 to 255 or 0.5 for a GrayF32 with values from 0.0 to 1.0.
 * </p>
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ThresholdTransformation implements Transformation {

    static final Logger logger = LogManager.getLogger(ThresholdTransformation.class);

    private final double threshold;

    public ThresholdTransformation(double threshold) {
        this.threshold = threshold;
    }

    @Override
    public String getName() {
        return this.getClass().getSimpleName();
    }

    @Override
    public ImageBase<?> transform(ImageBase<?> inputImage) throws TransformationException {
        try {
            return GThresholdImageOps.threshold((ImageGray) inputImage, null, this.getThreshold(), false);
        } catch (ClassCastException e) {
            throw new TransformationException("Input image must be an ImageGray", e);
        }
    }

    public double getThreshold() {
        return this.threshold;
    }

}
