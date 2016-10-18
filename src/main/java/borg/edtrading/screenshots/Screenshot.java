package borg.edtrading.screenshots;

import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.Planar;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.util.ImageUtil;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

/**
 * Full screenshot
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Screenshot {

    private final File file;
    private final Planar<GrayU8> originalImage;
    private final Planar<GrayU8> resizedImage;

    private Screenshot(File file, Planar<GrayU8> originalImage, Planar<GrayU8> resizedImage) {
        this.file = file;
        this.originalImage = originalImage;
        this.resizedImage = resizedImage;
    }

    public static Screenshot loadFromFile(File file, int targetWidth, int targetHeight, Screenshot previosScreenshot) throws IOException {
        // Load as BufferedImage
        BufferedImage bi = ImageIO.read(file);

        // If we have a previous screenshot of the same format we can re-use memory
        boolean sameFormatAsPrevious = false;
        if (previosScreenshot != null) {
            Planar<GrayU8> p = previosScreenshot.getOriginalImage();
            sameFormatAsPrevious = bi.getWidth() == p.getWidth() && bi.getHeight() == p.getHeight() && bi.getRaster().getNumBands() == p.getNumBands();
        }

        // Convert into planar (A)RGB
        Planar<GrayU8> originalImage = sameFormatAsPrevious ? previosScreenshot.getOriginalImage() : new Planar<>(GrayU8.class, bi.getWidth(), bi.getHeight(), bi.getRaster().getNumBands());
        ConvertBufferedImage.convertFromMulti(bi, originalImage, true, GrayU8.class);

        // Resize to target size and discard alpha channel
        Planar<GrayU8> rgb = originalImage.getNumBands() == 4 ? originalImage.partialSpectrum(1, 2, 3) : originalImage;
        Planar<GrayU8> resizedImage = sameFormatAsPrevious ? previosScreenshot.getResizedImage() : new Planar<>(GrayU8.class, targetWidth, targetHeight, 3);
        for (int band = 0; band < 3; band++) {
            ImageUtil.cropAndScaleTo(rgb.getBand(band), resizedImage.getBand(band));
        }

        return new Screenshot(file, originalImage, resizedImage);
    }

    public void saveToFile(File file) throws IOException {
        this.getAsRegion().saveToFile(file, Transformation.ORIGINAL);
    }

    @Override
    public String toString() {
        return this.getFile().getName() + " (" + this.getOriginalWidth() + "x" + this.getOriginalHeight() + " -> " + this.getResizedWidth() + "x" + this.getResizedHeight() + ")";
    }

    public Region getAsRegion() {
        return this.getRegion(0, 0, this.getResizedWidth(), this.getResizedHeight());
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
