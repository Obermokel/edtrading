package borg.edtrading.ocr.fixer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * RadiusFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GravityFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(GravityFixer.class);

    @Override
    public String fixValue(String scannedText) {
        String fixedValue = scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(",", ".");
        if (fixedValue.contains(".") && fixedValue.indexOf(".") == fixedValue.length() - 4) {
            fixedValue = fixedValue.substring(0, fixedValue.length() - 1) + "G";
        }
        return fixedValue;
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\d\\.\\d{2}G"); // min 0.00G, max 9.99G, always two decimal places
    }

}
