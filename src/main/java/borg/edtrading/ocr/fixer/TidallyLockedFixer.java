package borg.edtrading.ocr.fixer;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * TidallyLockedFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TidallyLockedFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(TidallyLockedFixer.class);

    @Override
    public String fixValue(String scannedText) {
        return scannedText;
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return StringUtils.isEmpty(fixedValue);
    }

}
