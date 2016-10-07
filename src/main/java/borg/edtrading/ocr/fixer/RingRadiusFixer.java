package borg.edtrading.ocr.fixer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * RadiusFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RingRadiusFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(RingRadiusFixer.class);

    public RingRadiusFixer() {
    }

    @Override
    public String fixValue(String scannedText) {
        String fixedValue = scannedText.toUpperCase().replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(".", ",");
        if (fixedValue.contains(",") && fixedValue.indexOf(",") == fixedValue.length() - 6) {
            fixedValue = fixedValue.substring(0, fixedValue.length() - 2) + "KM";
        }
        return fixedValue;
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("(\\d{1,3},)?\\d{3}KM"); // min 100KM, max 999,999KM
    }

}
