package borg.edtrading.ocr.fixer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * RadiusFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SemiMajorAxisFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(SemiMajorAxisFixer.class);

    @Override
    public String fixValue(String scannedText) {
        return scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(",", ".");
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\d\\.\\d{2}AU"); // min 0.00AU, max 9.99AU, always two decimal places
    }

}
