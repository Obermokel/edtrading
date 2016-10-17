package borg.edtrading.ocr;

import boofcv.struct.image.GrayU8;
import boofcv.struct.image.Planar;

import java.io.File;

/**
 * Full screenshot
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Screenshot {

    private final File file;
    private final Planar<GrayU8> originalImage;
    private final Planar<GrayU8> resizedImage;

    public Screenshot(File file, Planar<GrayU8> originalImage, Planar<GrayU8> resizedImage) {
        this.file = file;
        this.originalImage = originalImage;
        this.resizedImage = resizedImage;
    }

    @Override
    public String toString() {
        return this.getFile().getName() + " (" + this.getOriginalWidth() + "x" + this.getOriginalHeight() + " -> " + this.getResizedWidth() + "x" + this.getResizedHeight() + ")";
    }

    public Region getRegion(int x, int y, int width, int height) {
        return new Region(this, x, y, this.resizedImage.subimage(x, y, x + width, y + height));
    }

    /**
     * Source file of this screenshot data
     */
    public File getFile() {
        return this.file;
    }

    /**
     * Original pixel data (ARGB or RGB) as loaded from the file. Should not really be worked with other than
     * for debugging purposes. All template matching etc should be done using the {@link #getResizedImage() resized image}.
     */
    public Planar<GrayU8> getOriginalImage() {
        return this.originalImage;
    }

    /**
     * Width in pixels of the original image
     */
    public int getOriginalWidth() {
        return this.originalImage.width;
    }

    /**
     * Height in pixels of the original image
     */
    public int getOriginalHeight() {
        return this.originalImage.height;
    }

    /**
     * Resized and cropped (for example to 3440x1440 to 3840x2160) pixel data, always RGB (i.e. without alpha channel).
     * This is the image data that should be used for template matching etc.
     */
    public Planar<GrayU8> getResizedImage() {
        return this.resizedImage;
    }

    /**
     * Width in pixels of the resized image
     */
    public int getResizedWidth() {
        return this.resizedImage.width;
    }

    /**
     * Height in pixels of the resized image
     */
    public int getResizedHeight() {
        return this.resizedImage.height;
    }

}
