package borg.edtrading.ocr;

import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Match;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * TextLine
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TextLine {

    static final Logger logger = LogManager.getLogger(TextLine.class);

    private final int xInScreenshot;
    private final int yInScreenshot;
    private final int width;
    private final int height;
    private final List<Match> matches;

    private TextLine(int xInScreenshot, int yInScreenshot, int width, int height, List<Match> matches) {
        this.xInScreenshot = xInScreenshot;
        this.yInScreenshot = yInScreenshot;
        this.width = width;
        this.height = height;
        this.matches = matches;
    }

    @Override
    public String toString() {
        return this.toText() + " (" + this.getxInScreenshot() + "/" + this.getyInScreenshot() + ", " + this.getWidth() + "x" + this.getHeight() + ")";
    }

    public String toText() {
        String text = "";

        int avgCharHeight = 0;
        for (Match m : this.getMatches()) {
            avgCharHeight += m.getRegion().getHeight();
        }
        avgCharHeight = avgCharHeight / this.getMatches().size();
        int minWordSpace = avgCharHeight / 2;
        int minKeyValueSpace = avgCharHeight * 2;

        int lastMatchEndX = -1;
        for (Match m : this.getMatches()) {
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

    public List<Match> getMatches() {
        return this.matches;
    }

    public static class TextLineBuilder {

        private final List<Word> words = new ArrayList<>();
        private final Screenshot screenshot;

        TextLineBuilder(Word seedingWord, Screenshot screenshot) {
            this.words.add(seedingWord);
            this.screenshot = screenshot;
        }

        /**
         * @return <code>true</code> if the <code>candidateWord</code> has been added to the text line
         */
        public boolean addToTextLine(Word candidateWord) {
            Rectangle candidateRect = new Rectangle(candidateWord.getxInScreenshot(), candidateWord.getyInScreenshot(), candidateWord.getWidth() + this.screenshot.getResizedWidth() / 10, candidateWord.getHeight());
            for (Word existingWord : this.words) {
                Rectangle existingRect = new Rectangle(existingWord.getxInScreenshot(), existingWord.getyInScreenshot(), existingWord.getWidth() + this.screenshot.getResizedWidth() / 10, existingWord.getHeight());
                if (candidateRect.intersects(existingRect)) {
                    this.words.add(candidateWord);
                    return true;
                }
            }
            return false;
        }

        public TextLine build() {
            int minX = 999999999;
            int minY = 999999999;
            int maxX = -999999999;
            int maxY = -999999999;
            List<Match> matches = new ArrayList<>();
            for (Word w : this.words) {
                minX = Math.min(minX, w.getxInScreenshot());
                minY = Math.min(minY, w.getyInScreenshot());
                maxX = Math.max(maxX, w.getxInScreenshot() + w.getWidth());
                maxY = Math.max(maxY, w.getyInScreenshot() + w.getHeight());
                matches.addAll(w.getSortedMatches());
            }
            Collections.sort(matches, new Comparator<Match>() {
                @Override
                public int compare(Match m1, Match m2) {
                    return new Integer(m1.getxInScreenshot()).compareTo(new Integer(m2.getxInScreenshot()));
                }
            });
            return new TextLine(minX, minY, maxX - minX, maxY - minY, matches);
        }

    }

}
