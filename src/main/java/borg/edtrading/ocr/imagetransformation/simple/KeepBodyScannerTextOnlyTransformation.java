package borg.edtrading.ocr.imagetransformation.simple;

import boofcv.alg.color.ColorHsv;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageBase;
import boofcv.struct.image.Planar;
import borg.edtrading.ocr.imagetransformation.Transformation;
import borg.edtrading.ocr.imagetransformation.TransformationException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * <p>
 * Input Image must be a <code>Planar&lt;GrayU8&gt;</code> with values between 0 and 255.
 * </p>
 * <p>
 * Output Image will be <code>Planar&lt;GrayU8&gt;</code> with values between 0 and 255.
 * </p>
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class KeepBodyScannerTextOnlyTransformation implements Transformation {

    static final Logger logger = LogManager.getLogger(KeepBodyScannerTextOnlyTransformation.class);

    @Override
    public ImageBase<?> transform(ImageBase<?> inputImage) throws TransformationException {
        if (!(inputImage instanceof Planar<?>)) {
            throw new TransformationException("inputImage must be a Planar");
        } else {
            Planar<?> inputPlanar = (Planar<?>) inputImage;

            if (inputPlanar.getNumBands() != 3) {
                throw new TransformationException("inputImage must have 3 bands");
            } else if (!inputPlanar.getBandType().isAssignableFrom(GrayU8.class)) {
                throw new TransformationException("inputImage band type must be GrayU8");
            } else {
                Planar<GrayU8> inputPlanarU8 = (Planar<GrayU8>) inputPlanar;
                Planar<GrayF32> rgbF32 = normalizeU8toF32(inputPlanarU8);
                Planar<GrayF32> hsvF32 = new Planar<>(GrayF32.class, inputImage.width, inputImage.height, 3);
                Planar<GrayU8> outputPlanarU8 = new Planar<GrayU8>(GrayU8.class, inputImage.width, inputImage.height, 3);

                // RGB -> HSV
                ColorHsv.rgbToHsv_F32(rgbF32, hsvF32);

                // Keep white text only
                //float h = 0f; // angle in radians
                float s = 0f; // 0..1
                float v = 1f; // 0..1
                GrayU8 Rin = inputPlanarU8.getBand(0);
                GrayU8 Gin = inputPlanarU8.getBand(1);
                GrayU8 Bin = inputPlanarU8.getBand(2);
                //GrayF32 H = hsvF32.getBand(0);
                GrayF32 S = hsvF32.getBand(1);
                GrayF32 V = hsvF32.getBand(2);
                GrayU8 Rout = outputPlanarU8.getBand(0);
                GrayU8 Gout = outputPlanarU8.getBand(1);
                GrayU8 Bout = outputPlanarU8.getBand(2);
                for (int y = 0; y < inputImage.height; y++) {
                    for (int x = 0; x < inputImage.width; x++) {
                        float ds = Math.abs(S.unsafe_get(x, y) - s);
                        float dv = Math.abs(V.unsafe_get(x, y) - v);

                        if (ds <= 0.25f && dv <= 0.35f) {
                            Rout.unsafe_set(x, y, Rin.unsafe_get(x, y));
                            Gout.unsafe_set(x, y, Gin.unsafe_get(x, y));
                            Bout.unsafe_set(x, y, Bin.unsafe_get(x, y));
                        } else {
                            Rout.unsafe_set(x, y, 0);
                            Gout.unsafe_set(x, y, 0);
                            Bout.unsafe_set(x, y, 0);
                        }
                    }
                }
                return outputPlanarU8;
            }
        }
    }

    private static Planar<GrayF32> normalizeU8toF32(Planar<GrayU8> src) {
        Planar<GrayF32> dst = new Planar<>(GrayF32.class, src.width, src.height, 3);
        for (int band = 0; band < 3; band++) {
            GrayU8 u8 = src.getBand(band);
            GrayF32 f32 = new GrayF32(src.width, src.height);
            for (int y = 0; y < src.height; y++) {
                for (int x = 0; x < src.width; x++) {
                    int v = u8.unsafe_get(x, y);
                    f32.unsafe_set(x, y, v / 255f);
                }
            }
            dst.setBand(band, f32);
        }
        return dst;
    }

}
