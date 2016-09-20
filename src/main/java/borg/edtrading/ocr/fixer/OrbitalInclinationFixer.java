package borg.edtrading.ocr.fixer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * RadiusFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class OrbitalInclinationFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(OrbitalInclinationFixer.class);

    @Override
    public String fixValue(String scannedText) {
        return scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("B", "8").replace(",", ".");
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\-?\\d\\.\\d{2}°?"); // min -9.99°, max 9.99°, always two decimal places, ° optional
    }

}
