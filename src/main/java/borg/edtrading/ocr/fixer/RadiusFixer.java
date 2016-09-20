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
        return scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("B", "8").replace(".", ",");
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("(\\d,)?\\d{3}KM"); // Three digits followed by 'KM', or 4 digits with thousands separator, min 100KM, max 9,999KM
    }

}
