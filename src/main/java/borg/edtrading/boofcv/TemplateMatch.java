package borg.edtrading.boofcv;

import boofcv.struct.feature.Match;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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
