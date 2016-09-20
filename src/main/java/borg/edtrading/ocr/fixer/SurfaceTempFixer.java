package borg.edtrading.ocr.fixer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * RadiusFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SurfaceTempFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(SurfaceTempFixer.class);

    @Override
    public String fixValue(String scannedText) {
        String fixedValue = scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(".", ",");
        if (fixedValue.contains(",") && fixedValue.indexOf(",") == fixedValue.length() - 5) {
            fixedValue = fixedValue.substring(0, fixedValue.length() - 1) + "K";
        }
        return fixedValue;
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("(\\d,)?\\d{1,3}K"); // One to three digits followed by 'K', or 4 digits with thousands separator, min 0K, max 9,999K
    }

}
