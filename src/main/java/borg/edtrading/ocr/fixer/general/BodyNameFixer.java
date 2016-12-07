package borg.edtrading.ocr.fixer.general;

import borg.edtrading.ocr.fixer.AbstractFixer;
import borg.edtrading.ocr.fixer.FixerException;
import borg.edtrading.ocr.templatematching.Match;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;

/**
 * BodyNameFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyNameFixer extends AbstractFixer {

    static final Logger logger = LogManager.getLogger(BodyNameFixer.class);

    @Override
    public Object fix(List<Match> matches) throws FixerException {
        // TODO Auto-generated method stub
        return this.matchesToText(matches);
    }

}
