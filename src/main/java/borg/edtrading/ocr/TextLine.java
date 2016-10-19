package borg.edtrading.ocr;

import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Match;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

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
        List<TextLine> result = new ArrayList<>();

        if (!matches.isEmpty()) {
            // Assume all matches are from the same screenshot
            Screenshot screenshot = matches.get(0).getRegion().getScreenshot();

            // Identify avg char height which gives us a hint how much they may be cluttered around the y axis
            int avgCharHeight = 0;
            int firstCharY = screenshot.getResizedHeight();
            int lastCharY = 0;
            for (Match m : matches) {
                avgCharHeight += m.getRegion().getHeight();
                firstCharY = Math.min(firstCharY, m.getyInScreenshot());
                lastCharY = Math.max(lastCharY, m.getyInScreenshot());
            }
            avgCharHeight = avgCharHeight / matches.size();
            logger.debug(screenshot + ": avg char height = " + avgCharHeight + ", first char y = " + firstCharY + ", last char y = " + lastCharY);

            // Identify y coords which have many chars
            LinkedHashMap<Integer, Integer> numCharsByY = new LinkedHashMap<>(lastCharY - firstCharY);
            for (int y = firstCharY; y < lastCharY; y++) {
                int n = 0;
                for (Match m : matches) {
                    if (Math.abs(m.getyInScreenshot() - y) <= avgCharHeight / 2) {
                        n++;
                    }
                }
                numCharsByY.put(y, n);
            }
            MiscUtil.sortMapByValueReverse(numCharsByY);

            // Group matches by y
            Map<Integer, List<Match>> matchesByY = new LinkedHashMap<>();
            List<Match> remainingMatches = new ArrayList<>(matches);
            for (int y : numCharsByY.keySet()) {
                List<Match> yMatches = new ArrayList<>();
                ListIterator<Match> it = remainingMatches.listIterator();
                while (it.hasNext()) {
                    Match candidateMatch = it.next();
                    if (Math.abs(candidateMatch.getyInScreenshot() - y) <= avgCharHeight / 2) {
                        yMatches.add(candidateMatch);
                        it.remove();
                    }
                }
                if (!yMatches.isEmpty()) {
                    matchesByY.put(y, yMatches);
                }
            }
            logger.debug(screenshot + ": Grouped matches into " + matchesByY.size() + " y-coords");

            // Finally build text lines. If chars are at the same y-coord but very far apart x-coord wise, then build multiple text lines for that y-coord.
            for (int y : matchesByY.keySet()) {
                List<Match> yMatches = matchesByY.get(y);
                Collections.sort(yMatches, new Comparator<Match>() {
                    @Override
                    public int compare(Match m1, Match m2) {
                        return new Integer(m1.getxInScreenshot()).compareTo(new Integer(m2.getxInScreenshot()));
                    }
                });
                while (!yMatches.isEmpty()) {
                    int maxGapX = screenshot.getResizedWidth() / 10; // 10%

                    int lastX = yMatches.get(0).getxInScreenshot();
                    int minX = yMatches.get(0).getxInScreenshot();
                    int maxX = yMatches.get(0).getxInScreenshot() + yMatches.get(0).getRegion().getWidth();
                    int minY = yMatches.get(0).getyInScreenshot();
                    int maxY = yMatches.get(0).getyInScreenshot() + yMatches.get(0).getRegion().getHeight();
                    List<Match> textLineMatches = new ArrayList<>();

                    ListIterator<Match> it = yMatches.listIterator();
                    while (it.hasNext()) {
                        Match m = it.next();

                        if (m.getxInScreenshot() - lastX < maxGapX) {
                            // Line continues
                            textLineMatches.add(m);
                        } else {
                            // Line ends, next line begins
                            break;
                        }

                        lastX = m.getxInScreenshot();
                        minX = Math.min(minX, m.getxInScreenshot());
                        maxX = Math.max(maxX, m.getxInScreenshot() + m.getRegion().getWidth());
                        minY = Math.min(minY, m.getyInScreenshot());
                        maxY = Math.max(maxY, m.getyInScreenshot() + m.getRegion().getHeight());
                        it.remove();
                    }
                    result.add(new TextLine(screenshot, minX, minY, maxX - minX, maxY - minY, textLineMatches));
                }
            }

            Collections.sort(result, new Comparator<TextLine>() {
                @Override
                public int compare(TextLine tl1, TextLine tl2) {
                    return new Integer(tl1.getX()).compareTo(new Integer(tl2.getX()));
                }
            });
            Collections.sort(result, new Comparator<TextLine>() {
                @Override
                public int compare(TextLine tl1, TextLine tl2) {
                    return new Integer(tl1.getY()).compareTo(new Integer(tl2.getY()));
                }
            });
            for (TextLine tl : result) {
                logger.debug(screenshot + ": " + tl);
            }
        }

        return result;
    }

    @Override
    public String toString() {
        String text = "";
        for (Match m : this.getMatches()) {
            text += m.getTemplate().getText();
        }
        return text + " (" + this.getX() + "/" + this.getY() + ", " + this.getWidth() + "x" + this.getHeight() + ")";
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

}
