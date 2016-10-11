package borg.edtrading.boofcv;

import boofcv.struct.image.GrayF32;
import borg.edtrading.boofcv.TemplateMatcher.CroppedImage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Template
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Template {

    static final Logger logger = LogManager.getLogger(Template.class);

    private String text = null;
    private GrayF32 image = null;
    private CroppedImage croppedImage = null;
    private GrayF32 mask = null;

    public Template(String text, GrayF32 image, GrayF32 mask) {
        this.setText(text);
        this.setImage(image);
        this.setMask(mask);
    }

    @Override
    public String toString() {
        return "Template: '" + this.getText() + "'";
    }

    public String getText() {
        return this.text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public GrayF32 getImage() {
        return this.image;
    }

    public void setImage(GrayF32 image) {
        this.image = image;
    }

    public CroppedImage getCroppedImage() {
        return this.croppedImage;
    }

    public void setCroppedImage(CroppedImage croppedImage) {
        this.croppedImage = croppedImage;
    }

    public GrayF32 getMask() {
        return this.mask;
    }

    public void setMask(GrayF32 mask) {
        this.mask = mask;
    }

}
