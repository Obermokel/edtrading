package borg.edtrading.ocr.fixer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;

/**
 * RadiusFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ArgOfPeriapsisFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(ArgOfPeriapsisFixer.class);

    private static final BigDecimal MAX_VALUE = new BigDecimal("360.00");

    @Override
    public String fixValue(String scannedText) {
        return scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("B", "8").replace(",", ".");
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        // min 0.00°, max 359.99°, always two decimal places, ° optional
        if (!fixedValue.matches("\\d{1,3}\\.\\d{2}°?")) {
            return false;
        } else {
            BigDecimal actualValue = new BigDecimal(fixedValue.replace("°", ""));

            return actualValue.compareTo(MAX_VALUE) < 0;
        }
    }

}
