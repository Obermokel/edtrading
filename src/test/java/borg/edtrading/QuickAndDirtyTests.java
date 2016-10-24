package borg.edtrading;

import boofcv.core.image.ConvertImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageBase;
import borg.edtrading.imagetransformation.Transformation;
import borg.edtrading.imagetransformation.TransformationException;
import borg.edtrading.imagetransformation.simple.RgbToGrayTransformation;
import borg.edtrading.screenshots.Region;
import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Template;
import borg.edtrading.templatematching.TemplateMatcher;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * QuickAndDirtyTests
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class QuickAndDirtyTests {

    static final Logger logger = LogManager.getLogger(QuickAndDirtyTests.class);

    public static void main(String[] args) throws IOException {
        File file = new File(Constants.TEMPLATES_DIR, "CRAP\\CRAP\\CRAP#2016-10-03 22-56-11 Nogatas#255.1024.png");
        Screenshot screenshot = Screenshot.loadFromFile(file, 37, 23, null);
        Region region = screenshot.getAsRegion();
        region.applyTransformation("GRAY", new RgbToGrayTransformation());
        region.applyTransformation("F32", new Transformation() {
            @Override
            public ImageBase<?> transform(ImageBase<?> inputImage) throws TransformationException {
                return ConvertImage.convert((GrayU8) inputImage, (GrayF32) null);
            }
        });
        List<Template> templates = Template.fromFolder("BodyScanner");
        new TemplateMatcher().allNonOverlappingTemplates(region, templates);
    }

}
