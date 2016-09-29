package borg.edtrading.ocr.fixer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * RingMassFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RingMassFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(RingMassFixer.class);

    public RingMassFixer() {
    }

    @Override
    public String fixValue(String scannedText) {
        String fixedValue = scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8");
        if (fixedValue.contains(".") && fixedValue.indexOf(".") == fixedValue.length() - 4) {
            fixedValue = fixedValue.substring(0, fixedValue.length() - 2) + "MT";
        }
        return fixedValue;
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\d{1,3}(,\\d{3})+\\.\\dMT"); // min 1,000.0MT, max 999,999,999,999,999,999,999,999.9MT
    }

}
