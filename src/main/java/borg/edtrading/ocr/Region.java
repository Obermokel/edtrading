package borg.edtrading.ocr;

import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageBase;
import boofcv.struct.image.Planar;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Region (pixel area) of a full screenshot or another region
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Region {

    private final Screenshot parentScreenshot;
    private final Region parentRegion;
    private final int xInParent;
    private final int yInParent;
    private final Planar<GrayU8> originalRGB;
    private final LinkedHashMap<String, ImageBase<?>> transformed;

    Region(Screenshot parentScreenshot, int xInParent, int yInParent, Planar<GrayU8> originalRGB) {
        this.parentScreenshot = parentScreenshot;
        this.parentRegion = null;
        this.xInParent = xInParent;
        this.yInParent = yInParent;
        this.originalRGB = originalRGB;
        this.transformed = new LinkedHashMap<>();
    }

    Region(Region parentRegion, int xInParent, int yInParent, Planar<GrayU8> originalRGB, LinkedHashMap<String, ImageBase<?>> transformed) {
        this.parentScreenshot = null;
        this.parentRegion = parentRegion;
        this.xInParent = xInParent;
        this.yInParent = yInParent;
        this.originalRGB = originalRGB;
        this.transformed = transformed;
    }

    @Override
    public String toString() {
        return this.getWidth() + "x" + this.getHeight() + " region of " + this.getScreenshot();
    }

    private Object getParent() {
        return this.parentScreenshot != null ? this.parentScreenshot : this.parentRegion;
    }

    public Region getSubregion(int x, int y, int width, int height) {
        Planar<GrayU8> subregionOriginalRGB = this.originalRGB.subimage(x, y, x + width, y + height);
        LinkedHashMap<String, ImageBase<?>> subregionTransformed = new LinkedHashMap<>(this.transformed.size());
        for (String transformation : this.transformed.keySet()) {
            subregionTransformed.put(transformation, this.transformed.get(transformation).subimage(x, y, x + width, y + height));
        }
        return new Region(this, x, y, subregionOriginalRGB, subregionTransformed);
    }

    /**
     * RGB pixel data of this region
     */
    public Planar<GrayU8> getOriginalRGB() {
        return this.originalRGB;
    }

    /**
     * Transformed pixel data of this region
     *
     * @throws NoSuchTransformationException
     */
    public ImageBase<?> getTransformed(String transformation) throws NoSuchTransformationException {
        if (!this.transformed.containsKey(transformation)) {
            throw new NoSuchTransformationException(transformation);
        } else {
            return this.transformed.get(transformation);
        }
    }

    /**
     * Name of the transformations that have been applied to this region, in the order they have been applied
     */
    public List<String> getTransformations() {
        return new ArrayList<>(this.transformed.keySet());
    }

    /**
     * Apply a new transformation on the last tranformation
     *
     * @throws NoSuchTransformationException
     */
    public Region applyTransformation(Transformation t) throws NoSuchTransformationException {
        return this.applyTransformation(t, Transformation.APPLY_ON_LAST);
    }

    /**
     * Apply a new transformation on this region.
     *
     * @param t
     *      The transformation to be executed
     * @param applyOn
     *      On what input data to apply the transformation. If empty or {@link Transformation#APPLY_ON_ORIGINAL} it
     *      will be applied on the original RGB pixel data of this region, if {@link Transformation#APPLY_ON_LAST} it
     *      will be applied on top of the last transformation (which, in case none exists, is the original RGB data),
     *      or otherwise to the specified transformation.
     *
     * @throws NoSuchTransformationException
     */
    public Region applyTransformation(Transformation t, String applyOn) throws NoSuchTransformationException {
        ImageBase<?> inputImage = null;
        if (StringUtils.isEmpty(applyOn) || Transformation.APPLY_ON_ORIGINAL.equals(applyOn)) {
            inputImage = this.getOriginalRGB();
        } else if (Transformation.APPLY_ON_LAST.equals(applyOn)) {
            if (this.getTransformations().isEmpty()) {
                inputImage = this.getOriginalRGB();
            } else {
                inputImage = this.getTransformed(this.getTransformations().get(this.getTransformations().size() - 1));
            }
        } else {
            inputImage = this.getTransformed(applyOn);
        }

        ImageBase<?> transformedImage = t.transform(inputImage);
        this.transformed.put(t.getName(), transformedImage);
        return this;
    }

    /**
     * Screenshot that this region ultimately belongs to
     */
    public Screenshot getScreenshot() {
        Object parent = this.getParent();
        while (parent instanceof Region) {
            parent = ((Region) parent).getParent();
        }
        return (Screenshot) parent;
    }

    /**
     * X coord in the screenshot
     */
    public int getxInScreenshot() {
        int x = this.xInParent;
        Object parent = this.getParent();
        while (parent instanceof Region) {
            x += ((Region) parent).getxInParent();
            parent = ((Region) parent).getParent();
        }
        return x;
    }

    /**
     * Y coord in the screenshot
     */
    public int getyInScreenshot() {
        int y = this.yInParent;
        Object parent = this.getParent();
        while (parent instanceof Region) {
            y += ((Region) parent).getyInParent();
            parent = ((Region) parent).getParent();
        }
        return y;
    }

    /**
     * X coord in the parent
     */
    public int getxInParent() {
        return this.xInParent;
    }

    /**
     * Y coord in the parent
     */
    public int getyInParent() {
        return this.yInParent;
    }

    /**
     * Width in pixels of this region
     */
    public int getWidth() {
        return this.originalRGB.width;
    }

    /**
     * Height in pixels of this region
     */
    public int getHeight() {
        return this.originalRGB.height;
    }

}
