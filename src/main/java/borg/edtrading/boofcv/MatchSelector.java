package borg.edtrading.boofcv;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * MatchSelector
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MatchSelector {

    static final Logger logger = LogManager.getLogger(MatchSelector.class);

    public static List<TemplateMatch> selectMatches(List<TemplateMatch> matches) {
        sortByScore(matches);

        return selectNonOverlapping(matches);
    }

    private static List<TemplateMatch> selectNonOverlapping(List<TemplateMatch> matches) {
        logger.debug("Selecting from " + matches.size() + " match(es)");
        List<TemplateMatch> result = new ArrayList<>();

        for (TemplateMatch match : matches) {
            if (!match.overlapsWithAny(result)) {
                result.add(match);
            }
        }

        logger.debug("Selected " + result.size() + " match(es)");
        return result;
    }

    private static void sortByScore(List<TemplateMatch> matches) {
        logger.debug("Sorting " + matches.size() + " match(es) by score");
        Collections.sort(matches, new Comparator<TemplateMatch>() {
            @Override
            public int compare(TemplateMatch m1, TemplateMatch m2) {
                return new Integer(m1.getMatchQuality()).compareTo(new Integer(m2.getMatchQuality()));
            }
        });
    }

}
