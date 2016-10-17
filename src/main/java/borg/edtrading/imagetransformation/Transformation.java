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

    ImageBase<?> transform(ImageBase<?> inputImage);

}
