package borg.edtrading.imagetransformation.simple;

import boofcv.core.image.ConvertImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageBase;
import boofcv.struct.image.Planar;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.TransformationException;
import borg.edtrading.util.ImageUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * <p>
 * Input Image must be a <code>Planar&lt;GrayU8&gt;</code> with values between 0 and 255.
 * </p>
 * <p>
 * Output Image will be <code>GrayF32</code> with values between 0 and 1.
 * </p>
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RgbToGrayF32Transformation implements Transformation {

    static final Logger logger = LogManager.getLogger(RgbToGrayF32Transformation.class);

    @Override
    public ImageBase<?> transform(ImageBase<?> inputImage) throws TransformationException {
        try {
            Planar<GrayU8> inputPlanarU8 = (Planar<GrayU8>) inputImage;
            GrayU8 intermediateGrayU8 = ConvertImage.average(inputPlanarU8, null);
            GrayF32 outputGrayF32 = ConvertImage.convert(intermediateGrayU8, (GrayF32) null);
            return ImageUtil.normalize(outputGrayF32);
        } catch (ClassCastException e) {
            throw new TransformationException("Input image must be an Planar<GrayU8>", e);
        }
    }

}
