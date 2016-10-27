package borg.edtrading.ocr;

import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Match;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
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
    private final List<Word> sortedWords;

    private TextLine(int xInScreenshot, int yInScreenshot, int width, int height, List<Word> sortedWords) {
        this.xInScreenshot = xInScreenshot;
        this.yInScreenshot = yInScreenshot;
        this.width = width;
        this.height = height;
        this.sortedWords = sortedWords;
    }

    @Override
    public String toString() {
        return this.toText() + " (" + this.getxInScreenshot() + "/" + this.getyInScreenshot() + ", " + this.getWidth() + "x" + this.getHeight() + ")";
    }

    @Override
    public boolean equals(Object that) {
        return this == that || (that instanceof TextLine && this.toString().equals(that.toString()));
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    public String toText() {
        return this.toText(false);
    }

    public String toText(boolean preferShouldHaveBeen) {
        String text = "";
        Iterator<Word> it = this.getSortedWords().iterator();
        while (it.hasNext()) {
            text += it.next().toText(preferShouldHaveBeen);
            if (it.hasNext()) {
                text += " → ";
            }
        }
        return text;
    }

    public List<Match> getMatches() {
        List<Match> result = new ArrayList<>();
        for (Word w : this.getSortedWords()) {
            result.addAll(w.getSortedMatches());
        }
        return result;
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

    public List<Word> getSortedWords() {
        return this.sortedWords;
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
            for (Word w : this.words) {
                minX = Math.min(minX, w.getxInScreenshot());
                minY = Math.min(minY, w.getyInScreenshot());
                maxX = Math.max(maxX, w.getxInScreenshot() + w.getWidth());
                maxY = Math.max(maxY, w.getyInScreenshot() + w.getHeight());
            }
            Collections.sort(this.words, new Comparator<Word>() {
                @Override
                public int compare(Word w1, Word w2) {
                    return new Integer(w1.getxInScreenshot()).compareTo(new Integer(w2.getxInScreenshot()));
                }
            });
            return new TextLine(minX, minY, maxX - minX, maxY - minY, this.words);
        }

    }

}
