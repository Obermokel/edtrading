package borg.edtrading.boofcv;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.List;

import boofcv.io.image.UtilImageIO;
import boofcv.struct.image.ImageFloat32;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * TemplateLoader
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TemplateLoader {

    static final Logger logger = LogManager.getLogger(TemplateLoader.class);

    public static List<Template> loadTemplates() {
        List<Template> result = new ArrayList<>();

        File baseDir = new File(System.getProperty("user.home"), "EliteDangerous\\Templates");
        File[] subDirs = baseDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.isDirectory();
            }
        });
        for (File subDir : subDirs) {
            String text = subDir.getName();
            File[] pngFiles = subDir.listFiles(new FileFilter() {
                @Override
                public boolean accept(File file) {
                    return file.getName().toLowerCase().endsWith(".png");
                }
            });
            for (File pngFile : pngFiles) {
                ImageFloat32 image = UtilImageIO.loadImage(pngFile.getAbsolutePath(), ImageFloat32.class);
                result.add(new Template(text, image));
            }
        }

        return result;
    }

}
