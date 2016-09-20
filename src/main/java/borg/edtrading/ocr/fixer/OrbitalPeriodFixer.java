package borg.edtrading.ocr.fixer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * RadiusFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class OrbitalPeriodFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(OrbitalPeriodFixer.class);

    @Override
    public String fixValue(String scannedText) {
        String fixedValue = scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("B", "8").replace(",", ".");
        if (fixedValue.indexOf(".") == fixedValue.length() - 3) {
            fixedValue = fixedValue.substring(0, fixedValue.length() - 1) + "D";
        }
        return fixedValue;
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\d{1,3}\\.\\dD"); // min 0.0D, max 999.9D, always one decimal place
    }

}
