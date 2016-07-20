package borg.edtrading.boofcv;

import boofcv.struct.feature.Match;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Rectangle;
import java.util.List;

/**
 * TemplateMatch
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TemplateMatch {

    static final Logger logger = LogManager.getLogger(TemplateMatch.class);

    private Template template = null;
    private Match match = null;

    public TemplateMatch(Template template, Match match) {
        this.setTemplate(template);
        this.setMatch(match);
    }

    /**
     * The closer to 0 the better
     */
    public int getErrorPerPixel() {
        int pixels = this.getTemplate().getImage().getWidth() * this.getTemplate().getImage().getHeight();
        double error = -1 * this.getMatch().score;

        return (int) (error / pixels);
    }

    /**
     * The closer to 0 the better
     */
    public int getMatchQuality() {
        return this.getErrorPerPixel() / this.getTemplate().getImage().getWidth();
    }

    public boolean overlapsWithAny(List<TemplateMatch> other) {
        //@formatter:off

        // 1px smaller than actual in order to correct template match jitter
        final Rectangle thisRectangle = new Rectangle(
                this.getMatch().x + 1, this.getMatch().y + 1,
                this.getTemplate().getImage().width - 2, this.getTemplate().getImage().height - 2);

//        // Real size
//        final Rectangle thisRectangle = new Rectangle(
//                this.getMatch().x, this.getMatch().y,
//                this.getTemplate().getImage().width, this.getTemplate().getImage().height);

        //@formatter:on

        for (TemplateMatch that : other) {
            //@formatter:off

//            // 1px smaller than actual in order to correct template match jitter
//            final Rectangle thatRectangle = new Rectangle(
//                    that.getMatch().x + 1, that.getMatch().y + 1,
//                    that.getTemplate().getImage().width - 2, that.getTemplate().getImage().height - 2);

            // Real size
            final Rectangle thatRectangle = new Rectangle(
                    that.getMatch().x, that.getMatch().y,
                    that.getTemplate().getImage().width, that.getTemplate().getImage().height);

            //@formatter:on

            if (thisRectangle.intersects(thatRectangle)) {
                return true;
            }
        }

        return false;
    }

    public Template getTemplate() {
        return this.template;
    }

    public void setTemplate(Template template) {
        this.template = template;
    }

    public Match getMatch() {
        return this.match;
    }

    public void setMatch(Match match) {
        this.match = match;
    }

}
