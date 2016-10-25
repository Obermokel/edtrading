package borg.edtrading.ocr;

import borg.edtrading.bodyscanner.BodyScanner;
import borg.edtrading.templatematching.Match;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * A sorted sequence of template matches which are very close to each other, therefore
 * representing one or more words. The next level of match grouping would be a {@link TextLine}
 * which can have a large horizontal gap between a label and a value.
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Word {

    static final Logger logger = LogManager.getLogger(Word.class);

    private final int xInScreenshot;
    private final int yInScreenshot;
    private final int width;
    private final int height;
    private final List<Match> sortedMatches;

    private Word(int xInScreenshot, int yInScreenshot, int width, int height, List<Match> sortedMatches) {
        this.xInScreenshot = xInScreenshot;
        this.yInScreenshot = yInScreenshot;
        this.width = width;
        this.height = height;
        this.sortedMatches = sortedMatches;
    }

    @Override
    public String toString() {
        return this.toText() + " (" + this.getxInScreenshot() + "/" + this.getyInScreenshot() + ", " + this.getWidth() + "x" + this.getHeight() + ")";
    }

    public String toText() {
        String text = "";

        int avgCharHeight = 0;
        for (Match m : this.getSortedMatches()) {
            avgCharHeight += m.getRegion().getHeight();
        }
        avgCharHeight = avgCharHeight / this.getSortedMatches().size();
        int minWordSpace = avgCharHeight / 2;
        int minKeyValueSpace = avgCharHeight * 2;

        int lastMatchEndX = -1;
        for (Match m : this.getSortedMatches()) {
            if (lastMatchEndX >= 0) {
                int spaceToLast = m.getxInScreenshot() - lastMatchEndX;
                if (spaceToLast >= minKeyValueSpace) {
                    text += " → ";
                } else if (spaceToLast >= minWordSpace) {
                    text += " ";
                }
            }
            text += m.getTemplate().getText();
            lastMatchEndX = m.getxInScreenshot() + m.getRegion().getWidth();
        }
        return text;
    }

    public int getxInScreenshot() {
        return this.xInScreenshot;
    }

    public int getyInScreenshot() {
        return this.yInScreenshot;
    }

    public int getWidth() {
        return this.width;
    }

    public int getHeight() {
        return this.height;
    }

    public List<Match> getSortedMatches() {
        return this.sortedMatches;
    }

    public static class WordBuilder {

        private final List<Match> matches = new ArrayList<>();
        private final int avgCharHeight;

        public WordBuilder(Match seedingMatch, int avgCharHeight) {
            this.matches.add(seedingMatch);
            this.avgCharHeight = avgCharHeight;
        }

        /**
         * @return <code>true</code> if the <code>candidateMatch</code> has been added to the word
         */
        public boolean addToWord(Match candidateMatch) {
            Rectangle candidateRect = new Rectangle(candidateMatch.getxInScreenshot() - this.avgCharHeight, candidateMatch.getyInScreenshot(), candidateMatch.getWidth() + 2 * this.avgCharHeight, candidateMatch.getHeight());
            for (Match existingMatch : this.matches) {
                Rectangle existingRect = new Rectangle(existingMatch.getxInScreenshot() - this.avgCharHeight, existingMatch.getyInScreenshot(), existingMatch.getWidth() + 2 * this.avgCharHeight, existingMatch.getHeight());
                if (candidateRect.intersects(existingRect)) {
                    this.matches.add(candidateMatch);
                    return true;
                }
            }
            return false;
        }

        /**
         * @return <code>true</code> if only 1 match of 'known' quality has been added, or less than 50% of all matches are 'known'
         */
        public boolean looksLikeCrap() {
            float known = 0;
            for (Match m : this.matches) {
                known += m.getErrorPerPixel() <= BodyScanner.ERROR_PER_PIXEL_KNOWN ? 1 : 0;
            }
            float knownPercent = known / this.matches.size();
            return known <= 1 || knownPercent < 0.5f;
        }

        public Word build() {
            int minX = 999999999;
            int minY = 999999999;
            int maxX = -999999999;
            int maxY = -999999999;
            for (Match m : this.matches) {
                minX = Math.min(minX, m.getxInScreenshot());
                minY = Math.min(minY, m.getyInScreenshot());
                maxX = Math.max(maxX, m.getxInScreenshot() + m.getWidth());
                maxY = Math.max(maxY, m.getyInScreenshot() + m.getHeight());
            }
            Collections.sort(this.matches, new Comparator<Match>() {
                @Override
                public int compare(Match m1, Match m2) {
                    return new Integer(m1.getxInScreenshot()).compareTo(new Integer(m2.getxInScreenshot()));
                }
            });
            return new Word(minX, minY, maxX - minX, maxY - minY, this.matches);
        }

    }

}