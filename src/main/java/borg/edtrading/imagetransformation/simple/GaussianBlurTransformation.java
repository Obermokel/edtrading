package borg.edtrading.imagetransformation.simple;

import boofcv.alg.filter.blur.BlurImageOps;
import boofcv.core.image.ConvertImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageBase;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.TransformationException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * GaussianBlurTransformation
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GaussianBlurTransformation implements Transformation {

    static final Logger logger = LogManager.getLogger(GaussianBlurTransformation.class);

    private final int radius;
    private final double sigma;

    public GaussianBlurTransformation() {
        this(2, -1);
    }

    public GaussianBlurTransformation(int radius, double sigma) {
        this.radius = radius;
        this.sigma = sigma;
    }

    @Override
    public ImageBase<?> transform(ImageBase<?> inputImage) throws TransformationException {
        try {
            GrayF32 inputGrayF32 = inputImage instanceof GrayU8 ? ConvertImage.convert((GrayU8) inputImage, (GrayF32) null) : (GrayF32) inputImage;
            GrayF32 outputGrayF32 = inputGrayF32.createSameShape();
            BlurImageOps.gaussian(inputGrayF32, outputGrayF32, this.getSigma(), this.getRadius(), null);
            return outputGrayF32;
        } catch (ClassCastException e) {
            throw new TransformationException("Input image must be a GrayU8 or GrayF32", e);
        }
    }

    public int getRadius() {
        return this.radius;
    }

    public double getSigma() {
        return this.sigma;
    }

}
