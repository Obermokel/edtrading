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
import java.util.ListIterator;

/**
 * TextLine
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TextLine {

    static final Logger logger = LogManager.getLogger(TextLine.class);

    private final Screenshot screenshot;
    private final int x;
    private final int y;
    private final int width;
    private final int height;
    private final List<Match> matches;

    private TextLine(Screenshot screenshot, int x, int y, int width, int height, List<Match> matches) {
        this.screenshot = screenshot;
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
        this.matches = matches;
    }

    public static List<TextLine> matchesToTextLines(List<Match> matches) {
        if (matches.isEmpty()) {
            return Collections.emptyList();
        } else {
            // Assume all matches are from the same screenshot
            Screenshot screenshot = matches.get(0).getRegion().getScreenshot();

            // Identify avg char height which is a useful metric for further calculations
            int avgCharHeight = 0;
            int numAvgChars = 0;
            for (Match m : matches) {
                if (m.getTemplate().getText().matches("[0-9A-Z]")) {
                    avgCharHeight += m.getRegion().getHeight();
                    numAvgChars++;
                }
            }
            avgCharHeight = avgCharHeight / numAvgChars;
            logger.debug("avgCharHeight=" + avgCharHeight);

            // Find matches which are close to each other, therefore representing a word
            List<Word> words = new ArrayList<>();
            List<Match> remainingMatches = new ArrayList<>(matches);
            while (!remainingMatches.isEmpty()) {
                WordBuilder wb = new WordBuilder(remainingMatches.remove(0), avgCharHeight);

                boolean grown = false;
                do {
                    grown = false;
                    ListIterator<Match> it = remainingMatches.listIterator();
                    while (it.hasNext()) {
                        Match wordCandidate = it.next();
                        if (wb.addToWord(wordCandidate)) {
                            it.remove();
                            grown = true;
                        }
                    }
                } while (grown);

                if (wb.matches.size() > 1) {
                    words.add(wb.build());
                }
            }
            Collections.sort(words, new Comparator<Word>() {
                @Override
                public int compare(Word w1, Word w2) {
                    return new Integer(w1.xInScreenshot).compareTo(new Integer(w2.xInScreenshot));
                }
            });
            Collections.sort(words, new Comparator<Word>() {
                @Override
                public int compare(Word w1, Word w2) {
                    return new Integer(w1.yInScreenshot).compareTo(new Integer(w2.yInScreenshot));
                }
            });
            logger.debug("words[" + words.size() + "]=" + words);

            // Combine words to text lines
            List<TextLine> textLines = new ArrayList<>();
            List<Word> remainingWords = new ArrayList<>(words);
            while (!remainingWords.isEmpty()) {
                TextLineBuilder tlb = new TextLineBuilder(remainingWords.remove(0), screenshot);

                boolean grown = false;
                do {
                    grown = false;
                    ListIterator<Word> it = remainingWords.listIterator();
                    while (it.hasNext()) {
                        Word wordCandidate = it.next();
                        if (tlb.addToTextLine(wordCandidate)) {
                            it.remove();
                            grown = true;
                        }
                    }
                } while (grown);

                textLines.add(tlb.build());
            }
            Collections.sort(textLines, new Comparator<TextLine>() {
                @Override
                public int compare(TextLine tl1, TextLine tl2) {
                    return new Integer(tl1.x).compareTo(new Integer(tl2.x));
                }
            });
            Collections.sort(textLines, new Comparator<TextLine>() {
                @Override
                public int compare(TextLine tl1, TextLine tl2) {
                    return new Integer(tl1.y).compareTo(new Integer(tl2.y));
                }
            });
            logger.debug("textLines[" + textLines.size() + "]=" + textLines);

            return textLines;
        }
    }

    @Override
    public String toString() {
        return this.toText() + " (" + this.getX() + "/" + this.getY() + ", " + this.getWidth() + "x" + this.getHeight() + ")";
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

    public Screenshot getScreenshot() {
        return this.screenshot;
    }

    public int getX() {
        return this.x;
    }

    public int getY() {
        return this.y;
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

    static class TextLineBuilder {
        List<Word> words = new ArrayList<>();
        Screenshot screenshot = null;

        TextLineBuilder(Word seedingWord, Screenshot screenshot) {
            this.words.add(seedingWord);
            this.screenshot = screenshot;
        }

        boolean addToTextLine(Word candidateWord) {
            Rectangle candidateRect = new Rectangle(candidateWord.xInScreenshot, candidateWord.yInScreenshot, candidateWord.width + this.screenshot.getResizedWidth() / 10, candidateWord.height);
            for (Word existingWord : this.words) {
                Rectangle existingRect = new Rectangle(existingWord.xInScreenshot, existingWord.yInScreenshot, existingWord.width + this.screenshot.getResizedWidth() / 10, existingWord.height);
                if (candidateRect.intersects(existingRect)) {
                    this.words.add(candidateWord);
                    return true;
                }
            }
            return false;
        }

        TextLine build() {
            int minX = 999999999;
            int minY = 999999999;
            int maxX = -999999999;
            int maxY = -999999999;
            List<Match> matches = new ArrayList<>();
            for (Word w : this.words) {
                minX = Math.min(minX, w.xInScreenshot);
                minY = Math.min(minY, w.yInScreenshot);
                maxX = Math.max(maxX, w.xInScreenshot + w.width);
                maxY = Math.max(maxY, w.yInScreenshot + w.height);
                matches.addAll(w.sortedMatches);
            }
            Collections.sort(matches, new Comparator<Match>() {
                @Override
                public int compare(Match m1, Match m2) {
                    return new Integer(m1.getxInScreenshot()).compareTo(new Integer(m2.getxInScreenshot()));
                }
            });
            return new TextLine(this.screenshot, minX, minY, maxX - minX, maxY - minY, matches);
        }
    }

    static class WordBuilder {
        List<Match> matches = new ArrayList<>();
        int avgCharHeight = 0;

        WordBuilder(Match seedingMatch, int avgCharHeight) {
            this.matches.add(seedingMatch);
            this.avgCharHeight = avgCharHeight;
        }

        boolean addToWord(Match candidateMatch) {
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

        Word build() {
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

    static class Word {
        final int xInScreenshot;
        final int yInScreenshot;
        final int width;
        final int height;
        final List<Match> sortedMatches;

        Word(int xInScreenshot, int yInScreenshot, int width, int height, List<Match> sortedMatches) {
            this.xInScreenshot = xInScreenshot;
            this.yInScreenshot = yInScreenshot;
            this.width = width;
            this.height = height;
            this.sortedMatches = sortedMatches;
        }

        @Override
        public String toString() {
            String s = "";
            for (Match m : this.sortedMatches) {
                s += m.getTemplate().getText();
            }
            return s;
        }
    }

}
