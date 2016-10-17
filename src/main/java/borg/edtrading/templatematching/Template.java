package borg.edtrading.templatematching;

import boofcv.abst.distort.FDistort;
import boofcv.alg.interpolate.TypeInterpolate;
import boofcv.struct.image.GrayF32;

import java.io.File;

/**
 * Template which can be matched against screenshots or regions of screenshots
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Template {

    private final File file;
    private final String transformation;
    private final GrayF32 pixels;
    private final String text;

    public Template(File file, String transformation, GrayF32 pixels, String text) {
        this.file = file;
        this.transformation = transformation;
        this.pixels = pixels;
        this.text = text;
    }

    @Override
    public String toString() {
        return this.getText() + " (" + this.getWidth() + "x" + this.getHeight() + ")";
    }

    public GrayF32 scalePixelsToSize(int width, int height) {
        GrayF32 scaled = new GrayF32(width, height);
        new FDistort().input(this.getPixels()).output(scaled).interp(TypeInterpolate.BICUBIC).scale().apply();
        return scaled;
    }

    /**
     * Source file of this template data
     */
    public File getFile() {
        return this.file;
    }

    /**
     * Name of the transformation that has been used to create the pixel data of this template
     */
    public String getTransformation() {
        return this.transformation;
    }

    /**
     * Pre-processed image data of the template file
     */
    public GrayF32 getPixels() {
        return this.pixels;
    }

    /**
     * Text, that this template represents
     */
    public String getText() {
        return this.text;
    }

    /**
     * Width in pixels of this template
     */
    public int getWidth() {
        return this.pixels.width;
    }

    /**
     * Height in pixels of this template
     */
    public int getHeight() {
        return this.pixels.height;
    }

}
