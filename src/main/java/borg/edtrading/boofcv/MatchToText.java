package borg.edtrading.boofcv;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * MatchToText
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MatchToText {

    static final Logger logger = LogManager.getLogger(MatchToText.class);

    public static void matchesToText(List<TemplateMatch> matches) {
        // Make a copy from which we can remove already used matches
        List<TemplateMatch> copy = new ArrayList<>(matches);

        SortedMap<Integer, List<TemplateMatch>> matchesByRow = new TreeMap<>();
        for (int y = 0; y < 1440; y += 10) {
            List<TemplateMatch> rowMatches = new ArrayList<>(0);
            int numChars = 0;
            int numDigits = 0;
            for (TemplateMatch match : copy) {
                if (match.getMatch().y >= (y - 10) && match.getMatch().y < (y + 10)) {
                    rowMatches.add(match);
                    if (match.getTemplate().getText().matches("[A-Z]")) {
                        numChars++;
                    } else if (match.getTemplate().getText().matches("[0-9]")) {
                        numDigits++;
                    }
                }
            }
            if (numChars >= 5 && numDigits >= 3) {
                matchesByRow.put(y, rowMatches);
                copy.removeAll(rowMatches);
            }
        }

        for (Integer row : matchesByRow.keySet()) {
            List<TemplateMatch> rowMatches = matchesByRow.get(row);
            Collections.sort(rowMatches, new Comparator<TemplateMatch>() {
                @Override
                public int compare(TemplateMatch m1, TemplateMatch m2) {
                    return new Integer(m1.getMatch().x).compareTo(new Integer(m2.getMatch().x));
                }
            });
            for (TemplateMatch match : rowMatches) {
                System.out.print(match.getTemplate().getText());
            }
            System.out.println();
        }
    }

}
