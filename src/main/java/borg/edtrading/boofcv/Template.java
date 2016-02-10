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

    public Template(String text, ImageFloat32 image) {
        this.setText(text);
        this.setImage(image);
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

}
