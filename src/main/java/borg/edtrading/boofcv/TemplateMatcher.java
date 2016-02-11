package borg.edtrading.boofcv;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import boofcv.alg.feature.detect.template.TemplateMatching;
import boofcv.factory.feature.detect.template.FactoryTemplateMatching;
import boofcv.factory.feature.detect.template.TemplateScoreType;
import boofcv.struct.feature.Match;
import boofcv.struct.image.ImageFloat32;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * TemplateMatcher
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TemplateMatcher {

    static final Logger logger = LogManager.getLogger(TemplateMatcher.class);

    /**
     * @param screenshotImage
     *            Must already be cropped and color converted
     * @return
     */
    public static List<TemplateMatch> findAllTemplateMatches(ImageFloat32 screenshotImage) {
        List<Template> templates = TemplateLoader.loadTemplates();

        logger.debug("Scanning for " + templates.size() + " template(s) in screenshot");
        List<TemplateMatch> allMatches = new ArrayList<>();
        for (Template template : templates) {
            allMatches.addAll(findTemplateInScreenshot(template, screenshotImage));
        }

        return allMatches;
    }

    private static List<TemplateMatch> findTemplateInScreenshot(Template template, ImageFloat32 screenshotImage) {
        TemplateMatching<ImageFloat32> matcher = FactoryTemplateMatching.createMatcher(TemplateScoreType.SUM_DIFF_SQ, ImageFloat32.class);

        // Find the points which match the template the best
        matcher.setTemplate(template.getImage(), null, 100);
        matcher.process(screenshotImage);

        int templatePixels = template.getImage().getWidth() * template.getImage().getHeight();
        //double maxScore = 735.0 * templatePixels;

        List<TemplateMatch> result = new ArrayList<>();
        ListIterator<Match> it = matcher.getResults().toList().listIterator();
        while (it.hasNext()) {
            Match match = it.next();
            //if (match.score < maxScore) {
            result.add(new TemplateMatch(template, match));
            //}
        }

        return result;
    }

}
