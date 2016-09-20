package borg.edtrading.ocr.fixer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * RadiusFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RadiusFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(RadiusFixer.class);

    @Override
    public String fixValue(String scannedText) {
        String fixedValue = scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(".", ",");
        if (fixedValue.contains(",") && fixedValue.indexOf(",") == fixedValue.length() - 6) {
            fixedValue = fixedValue.substring(0, fixedValue.length() - 2) + "KM";
        }
        return fixedValue;
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("(\\d,)?\\d{3}KM"); // Three digits followed by 'KM', or 4 digits with thousands separator, min 100KM, max 9,999KM
    }

}
