package borg.edtrading.boofcv;

import boofcv.struct.image.ImageFloat32;
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
    private ImageFloat32 image = null;
    private boolean special = false;

    public Template(String text, ImageFloat32 image, boolean special) {
        this.setText(text);
        this.setImage(image);
        this.setSpecial(special);
    }

    public String getText() {
        return this.text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public ImageFloat32 getImage() {
        return this.image;
    }

    public void setImage(ImageFloat32 image) {
        this.image = image;
    }

    public boolean isSpecial() {
        return this.special;
    }

    public void setSpecial(boolean special) {
        this.special = special;
    }

}
