package borg.edtrading.imagetransformation;

import boofcv.struct.image.ImageBase;

/**
 * Transformation
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface Transformation {

    String ORIGINAL = "%ORIGINAL%";
    String LAST = "%LAST%";

    String getName();

    /**
     * @throws TransformationException
     */
    ImageBase<?> transform(ImageBase<?> inputImage) throws TransformationException;

}
