package borg.edtrading.ocr.fixer;

import borg.edtrading.ocr.templatematching.Match;

import java.util.List;

/**
 * Fixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface Fixer {

    Object fix(List<Match> matches) throws FixerException;

}
