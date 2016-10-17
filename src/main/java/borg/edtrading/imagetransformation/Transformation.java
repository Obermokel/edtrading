package borg.edtrading.imagetransformation;

import boofcv.struct.image.ImageBase;

/**
 * Transformation
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface Transformation {

    String APPLY_ON_ORIGINAL = "%ORIGINAL%";
    String APPLY_ON_LAST = "%LAST%";

    String getName();

    ImageBase<?> transform(ImageBase<?> inputImage);

}
