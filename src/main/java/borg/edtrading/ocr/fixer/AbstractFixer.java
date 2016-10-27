package borg.edtrading.ocr.fixer;

import borg.edtrading.templatematching.Match;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;

/**
 * AbstractFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public abstract class AbstractFixer implements Fixer {

    static final Logger logger = LogManager.getLogger(AbstractFixer.class);

    protected String matchesToText(List<Match> matches) {
        String text = "";
        for (Match m : matches) {
            text += m.getTemplate().getText();
        }
        return text;
    }

}
