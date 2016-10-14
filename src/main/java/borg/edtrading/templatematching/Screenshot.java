package borg.edtrading.templatematching;

import boofcv.struct.image.GrayF32;

import java.io.File;

/**
 * Full screenshot
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Screenshot {

    private final File file;
    private final GrayF32 pixels;

    public Screenshot(File file, GrayF32 pixels) {
        this.file = file;
        this.pixels = pixels;
    }

    @Override
    public String toString() {
        return this.getFile().getName() + " (" + this.getWidth() + "x" + this.getHeight() + ")";
    }

    /**
     * Source file of this screenshot data
     */
    public File getFile() {
        return this.file;
    }

    /**
     * Pre-processed image data of the screenshot file
     */
    public GrayF32 getPixels() {
        return this.pixels;
    }

    /**
     * Width in pixels of this screenshot
     */
    public int getWidth() {
        return this.pixels.width;
    }

    /**
     * Height in pixels of this screenshot
     */
    public int getHeight() {
        return this.pixels.height;
    }

}
