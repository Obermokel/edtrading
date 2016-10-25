package borg.edtrading.ocr;

import borg.edtrading.ocr.TextLine.TextLineBuilder;
import borg.edtrading.ocr.Word.WordBuilder;
import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Match;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;

/**
 * TextBuilder
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TextBuilder {

    static final Logger logger = LogManager.getLogger(TextBuilder.class);

    public static List<TextLine> matchesToText(List<Match> matches) {
        if (matches.isEmpty()) {
            return Collections.emptyList();
        } else {
            // Identify avg char height which is a useful metric for further calculations
            int avgCharHeight = computeAvgCharHeight(matches);

            // Find matches which are close to each other, therefore representing a word
            List<Word> words = matchesToWords(matches, avgCharHeight);

            // Assume all matches are from the same screenshot
            Screenshot screenshot = matches.get(0).getRegion().getScreenshot();

            // Combine words to text lines
            List<TextLine> textLines = wordsToTextLines(words, screenshot);

            // Finished
            return textLines;
        }
    }

    private static List<TextLine> wordsToTextLines(List<Word> words, Screenshot screenshot) {
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
                return new Integer(tl1.getxInScreenshot()).compareTo(new Integer(tl2.getxInScreenshot()));
            }
        });
        Collections.sort(textLines, new Comparator<TextLine>() {
            @Override
            public int compare(TextLine tl1, TextLine tl2) {
                return new Integer(tl1.getyInScreenshot()).compareTo(new Integer(tl2.getyInScreenshot()));
            }
        });
        logger.debug("textLines[" + textLines.size() + "]=" + textLines);
        return textLines;
    }

    private static List<Word> matchesToWords(List<Match> matches, int avgCharHeight) {
        List<Word> words = new ArrayList<>();
        List<Match> remainingMatches = new ArrayList<>(matches);
        while (!remainingMatches.isEmpty()) {
            Match seedingMatch = remainingMatches.remove(0);
            WordBuilder wb = new WordBuilder(seedingMatch, avgCharHeight);

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

            if (!wb.looksLikeCrap()) {
                words.add(wb.build());
            }
        }
        Collections.sort(words, new Comparator<Word>() {
            @Override
            public int compare(Word w1, Word w2) {
                return new Integer(w1.getxInScreenshot()).compareTo(new Integer(w2.getxInScreenshot()));
            }
        });
        Collections.sort(words, new Comparator<Word>() {
            @Override
            public int compare(Word w1, Word w2) {
                return new Integer(w1.getyInScreenshot()).compareTo(new Integer(w2.getyInScreenshot()));
            }
        });
        logger.debug("words[" + words.size() + "]=" + words);
        return words;
    }

    private static int computeAvgCharHeight(List<Match> matches) {
        int avgCharHeight = 0;
        int numAvgChars = 0;
        // Try to use only digits and uppercase letters because they always go from the baseline to the top
        for (Match m : matches) {
            if (m.getTemplate().getText().matches("[0-9A-Z]")) {
                avgCharHeight += m.getRegion().getHeight();
                numAvgChars++;
            }
        }
        if (numAvgChars == 0) {
            // If none were found then simply use all chars
            for (Match m : matches) {
                avgCharHeight += m.getRegion().getHeight();
                numAvgChars++;
            }
        }
        avgCharHeight = avgCharHeight / numAvgChars;
        logger.debug("avgCharHeight=" + avgCharHeight);
        return avgCharHeight;
    }

}
