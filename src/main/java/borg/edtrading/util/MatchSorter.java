package borg.edtrading.util;

import borg.edtrading.boofcv.TemplateMatch;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;

/**
 * MatchSorter
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MatchSorter {

    static final Logger logger = LogManager.getLogger(MatchSorter.class);

    public static List<MatchGroup> sortMatches(Collection<TemplateMatch> matches) {
        List<TemplateMatch> sortedMatches = new ArrayList<TemplateMatch>(matches);

        // Sort by score, so the seeding matches are the best matches.
        Collections.sort(sortedMatches, new Comparator<TemplateMatch>() {
            @Override
            public int compare(TemplateMatch m1, TemplateMatch m2) {
                return -1 * new Double(m1.getMatch().score).compareTo(new Double(m2.getMatch().score));
            }
        });

        // Create MatchGroups.
        // This will pick the topmost match as a seeding match, then add surrounding matches
        // as long as possible, removing them from the list. The next topmost match will start
        // a new MatchGroup, until no single match is left in the list.
        List<MatchGroup> matchGroups = new ArrayList<>();
        while (addNextMatchGroup(sortedMatches, matchGroups)) {
            // Continue
        }

        // Sort the match groups by position.
        // High prio = y, low prio = x -> first sort by x, then by y.
        Collections.sort(matchGroups, new Comparator<MatchGroup>() {
            @Override
            public int compare(MatchGroup mg1, MatchGroup mg2) {
                return new Integer(mg1.getX()).compareTo(new Integer(mg2.getX()));
            }
        });
        Collections.sort(matchGroups, new Comparator<MatchGroup>() {
            @Override
            public int compare(MatchGroup mg1, MatchGroup mg2) {
                return new Integer(mg1.getY()).compareTo(new Integer(mg2.getY()));
            }
        });

        return matchGroups;
    }

    private static boolean addNextMatchGroup(List<TemplateMatch> sortedMatches, List<MatchGroup> matchGroups) {
        if (!sortedMatches.isEmpty()) {
            TemplateMatch seedingMatch = sortedMatches.remove(0);
            MatchGroup matchGroup = new MatchGroup(seedingMatch);
            while (matchGroup.expand(sortedMatches)) {
                // Continue
            }
            matchGroups.add(matchGroup);
            return true;
        }

        return false;
    }

    public static class MatchGroup {

        private final List<TemplateMatch> groupMatches = new ArrayList<>();
        private String text = null;
        private Integer x = null;
        private Integer y = null;

        public MatchGroup(TemplateMatch seedingMatch) {
            this.groupMatches.add(seedingMatch);
        }

        public boolean expand(List<TemplateMatch> remainingMatches) {
            ListIterator<TemplateMatch> it = remainingMatches.listIterator();
            while (it.hasNext()) {
                TemplateMatch groupMatchCandidate = it.next(); // This one might be added to the group
                for (TemplateMatch groupMatchConfirmed : this.groupMatches) { // These already have been added
                    if (this.areMatchesAdjacent(groupMatchCandidate, groupMatchConfirmed)) { // Check
                        this.groupMatches.add(groupMatchCandidate); // Add
                        it.remove(); // Remove
                        return true; // Continue
                    }
                }
            }

            return false; // No more adjacent matches -> Finish
        }

        private boolean areMatchesAdjacent(TemplateMatch m1, TemplateMatch m2) {
            final int h1 = m1.getTemplate().getImage().height;
            final int h2 = m2.getTemplate().getImage().height;
            final int y1 = m1.getMatch().y;
            final int y2 = m2.getMatch().y;
            int x1 = m1.getMatch().x;
            int x2 = m2.getMatch().x;
            int w1 = m1.getTemplate().getImage().width;
            int w2 = m2.getTemplate().getImage().width;
            if ("1".equals(m1.getTemplate().getText())) {
                int doubleWidth = w1 * 2;
                int twoThirdsLeft = Math.round(x1 - w1 * 0.667f);
                x1 = twoThirdsLeft;
                w1 = doubleWidth;
            }
            if ("1".equals(m2.getTemplate().getText())) {
                int doubleWidth = w2 * 2;
                int twoThirdsLeft = Math.round(x2 - w2 * 0.667f);
                x2 = twoThirdsLeft;
                w2 = doubleWidth;
            }

            // Check they are at the same line (y-axis)
            double maxYDiff = Math.max(h1, h2) / 4.0; // Max y-axis diff is 1/4th of the highest of both
            double avgYDiff = (Math.abs(y1 - y2) + Math.abs((y1 + h1) - (y2 + h2))) / 2.0;

            if (avgYDiff <= maxYDiff) {
                // Check there is only a small gap between them
                double maxXGap = Math.max(h1, h2) / 6.0; // Max x-axis gap is 1/6th of the highest of both
                double xGap1 = Math.abs(x2 - (x1 + w1));
                double xGap2 = Math.abs(x1 - (x2 + w2));
                double minXGap = Math.min(xGap1, xGap2);

                if (minXGap <= maxXGap) {
                    return true;
                }
            }

            return false;
        }

        public List<TemplateMatch> getGroupMatches() {
            return this.groupMatches;
        }

        public String getText() {
            if (this.text == null) {
                this.sortMatchesAndParseText();
            }

            return this.text;
        }

        public Integer getX() {
            if (this.x == null) {
                this.sortMatchesAndParseText();
            }

            return this.x;
        }

        public Integer getY() {
            if (this.y == null) {
                this.sortMatchesAndParseText();
            }

            return this.y;
        }

        private void sortMatchesAndParseText() {
            // All matches should belong to the same line -> sort only by x-axis
            Collections.sort(this.groupMatches, new Comparator<TemplateMatch>() {
                @Override
                public int compare(TemplateMatch m1, TemplateMatch m2) {
                    return new Integer(m1.getMatch().x).compareTo(new Integer(m2.getMatch().x));
                }
            });

            // Set upper-left corner of whole match group
            this.x = this.groupMatches.get(0).getMatch().x;
            this.y = this.groupMatches.get(0).getMatch().y;

            // Concat text (TODO: detect spaces)
            StringBuilder sb = new StringBuilder();
            for (TemplateMatch m : this.groupMatches) {
                sb.append(m.getTemplate().getText());
            }
            this.text = sb.toString();
        }

    }

}
