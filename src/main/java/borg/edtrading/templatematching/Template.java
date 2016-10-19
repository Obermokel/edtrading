package borg.edtrading.templatematching;

import boofcv.abst.distort.FDistort;
import boofcv.alg.interpolate.TypeInterpolate;
import boofcv.gui.image.VisualizeImageData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import borg.edtrading.Constants;
import borg.edtrading.screenshots.Region;
import borg.edtrading.util.ImageUtil;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.imageio.ImageIO;

/**
 * Template which can be matched against screenshots or regions of screenshots
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Template {

    private final File file;
    private final String transformation;
    private final GrayF32 pixels;
    // TODO mask
    private final String text;

    private Template(File file, String transformation, GrayF32 pixels, String text) {
        this.file = file;
        this.transformation = transformation;
        this.pixels = pixels;
        this.text = text;
    }

    public static Template createNewFromRegion(Region region, String templateSetName, String templateText) throws IOException {
        String lastTransformation = region.getTransformations().get(region.getTransformations().size() - 1);
        GrayF32 pixels = ImageUtil.normalize((GrayF32) region.getImageData(lastTransformation));
        File setDir = new File(Constants.TEMPLATES_DIR, templateSetName);
        File textDir = new File(setDir, textToFolder(templateText));
        textDir.mkdirs();
        String fn = region.getScreenshot().getFile().getName().substring(0, region.getScreenshot().getFile().getName().lastIndexOf("."));
        File file = new File(textDir, String.format("%s#%s#%d.%d#%s.png", textToFolder(templateText), fn, region.getxInScreenshot(), region.getyInScreenshot(), lastTransformation));
        ImageIO.write(VisualizeImageData.grayMagnitude(pixels, null, -1), "png", file);

        return new Template(file, lastTransformation, pixels, templateText);
    }

    public static Template fromFile(File file) throws IOException {
        String transformation = file.getName().substring(file.getName().lastIndexOf("#") + 1, file.getName().lastIndexOf("."));
        GrayF32 pixels = ImageUtil.normalize(ConvertBufferedImage.convertFrom(ImageIO.read(file), (GrayF32) null));
        String text = folderToText(file.getParentFile().getName());

        return new Template(file, transformation, pixels, text);
    }

    public static List<Template> fromFolder(String templateSetName) throws IOException {
        List<Template> result = new ArrayList<>();
        File setDir = new File(Constants.TEMPLATES_DIR, templateSetName);
        File[] textDirs = setDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.isDirectory();
            }
        });
        for (File textDir : textDirs) {
            File[] files = textDir.listFiles(new FileFilter() {
                @Override
                public boolean accept(File file) {
                    return file.getName().endsWith(".png") && !file.getName().endsWith("_mask.png");
                }
            });
            for (File file : files) {
                result.add(Template.fromFile(file));
            }
        }
        return result;
    }

    @Override
    public String toString() {
        return this.getText() + " (" + this.getWidth() + "x" + this.getHeight() + ")";
    }

    public GrayF32 scalePixelsToSize(int width, int height) {
        if (this.getPixels().getWidth() == width && this.getPixels().getHeight() == height) {
            return this.getPixels();
        } else {
            GrayF32 scaled = new GrayF32(width, height);
            new FDistort().input(this.getPixels()).output(scaled).interp(TypeInterpolate.BICUBIC).scaleExt().apply();
            return scaled;
        }
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

    private static String folderToText(String folder) {
        String text = folder;
        if (folder.endsWith("_")) {
            text = text.substring(0, text.length() - 1).toLowerCase();
        } else if ("_punkt".equals(folder)) {
            text = ".";
        } else if ("_komma".equals(folder)) {
            text = ",";
        } else if ("_apostroph".equals(folder)) {
            text = "'";
        } else if ("_mikro".equals(folder)) {
            text = "µ";
        } else if ("_prozent".equals(folder)) {
            text = "%";
        } else if ("_strich".equals(folder)) {
            text = "-";
        } else if ("_plus".equals(folder)) {
            text = "+";
        } else if ("_doppelpunkt".equals(folder)) {
            text = ":";
        } else if ("_klammer_auf".equals(folder)) {
            text = "(";
        } else if ("_klammer_zu".equals(folder)) {
            text = ")";
        } else if ("_grad".equals(folder)) {
            text = "°";
        } else if ("_crap".equals(folder)) {
            text = "▪";
        }
        return text;
    }

    private static String textToFolder(String text) {
        String folder = text;
        if (text.matches("[a-z]+")) {
            folder = folder + "_";
        } else if (".".equals(text)) {
            folder = "_punkt";
        } else if (",".equals(text)) {
            folder = "_komma";
        } else if ("'".equals(text)) {
            folder = "_apostroph";
        } else if ("µ".equals(text)) {
            folder = "_mikro";
        } else if ("%".equals(text)) {
            folder = "_prozent";
        } else if ("-".equals(text)) {
            folder = "_strich";
        } else if ("+".equals(text)) {
            folder = "_plus";
        } else if (":".equals(text)) {
            folder = "_doppelpunkt";
        } else if ("(".equals(text)) {
            folder = "_klammer_auf";
        } else if (")".equals(text)) {
            folder = "_klammer_zu";
        } else if ("°".equals(text)) {
            folder = "_grad";
        } else if ("▪".equals(text)) {
            folder = "_crap";
        }
        return folder;
    }

}
