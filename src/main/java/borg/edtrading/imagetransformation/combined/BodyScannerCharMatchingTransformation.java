package borg.edtrading.imagetransformation.combined;

import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageBase;
import boofcv.struct.image.Planar;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.TransformationException;
import borg.edtrading.imagetransformation.simple.GaussianBlurTransformation;
import borg.edtrading.imagetransformation.simple.KeepBodyScannerTextOnlyTransformation;
import borg.edtrading.imagetransformation.simple.RgbToGrayTransformation;
import borg.edtrading.imagetransformation.simple.ThresholdTransformation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * BodyScannerCharMatchingTransformation
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyScannerCharMatchingTransformation implements Transformation {

    static final Logger logger = LogManager.getLogger(BodyScannerCharMatchingTransformation.class);

    private final boolean applyBlur;

    public BodyScannerCharMatchingTransformation(boolean applyBlur) {
        this.applyBlur = applyBlur;
    }

    @Override
    public String getName() {
        return "BSCM";
    }

    @Override
    public ImageBase<?> transform(ImageBase<?> inputImage) throws TransformationException {
        Planar<GrayU8> text = (Planar<GrayU8>) new KeepBodyScannerTextOnlyTransformation().transform(inputImage);
        GrayU8 gray = (GrayU8) new RgbToGrayTransformation().transform(text);
        GrayU8 threshold = (GrayU8) new ThresholdTransformation(128).transform(gray);
        if (this.applyBlur) {
            return (GrayF32) new GaussianBlurTransformation(2, -1).transform(threshold);
        } else {
            return threshold;
        }
    }

}
