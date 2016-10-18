package borg.edtrading.imagetransformation.combined;

import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageBase;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.TransformationException;
import borg.edtrading.imagetransformation.simple.CannyEdgeTransformation;
import borg.edtrading.imagetransformation.simple.GaussianBlurTransformation;
import borg.edtrading.imagetransformation.simple.RgbToGrayTransformation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * BodyScannerFeatureLocatorTransformation
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyScannerFeatureLocatorTransformation implements Transformation {

    static final Logger logger = LogManager.getLogger(BodyScannerFeatureLocatorTransformation.class);

    @Override
    public String getName() {
        return "BSFL";
    }

    @Override
    public ImageBase<?> transform(ImageBase<?> inputImage) throws TransformationException {
        GrayU8 gray = (GrayU8) new RgbToGrayTransformation().transform(inputImage);
        GrayU8 canny = (GrayU8) new CannyEdgeTransformation(2, 0.033f, 0.1f).transform(gray);
        GrayF32 gaussian = (GrayF32) new GaussianBlurTransformation(2, -1).transform(canny);
        return gaussian;
    }

}
