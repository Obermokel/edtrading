package borg.edtrading.ocr.fixer;

import borg.edtrading.data.BodyInfo;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * VolcanismFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RingTypeFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(RingTypeFixer.class);

    @Override
    public String fixValue(String scannedText) {
        String fixedValue = scannedText.toUpperCase().replace("0", "O").replace("5", "S").replace("8", "B");
        BodyInfo bodyInfo = BodyInfo.findBestMatching(fixedValue, "RING_TYPE_");
        if (bodyInfo != null) {
            fixedValue = bodyInfo.getName().toUpperCase();
        }
        return fixedValue.replaceAll("\\s", "");
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        BodyInfo bodyInfo = BodyInfo.findBestMatching(fixedValue, "RING_TYPE_");

        return bodyInfo != null;
    }

}
