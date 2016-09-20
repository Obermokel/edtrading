package borg.edtrading.ocr.fixer;

import borg.edtrading.data.BodyInfo;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * VolcanismFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class AtmosphereTypeFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(AtmosphereTypeFixer.class);

    @Override
    public String fixValue(String scannedText) {
        String fixedValue = scannedText.toUpperCase().replace("0", "O").replace("8", "B");
        BodyInfo bodyInfo = BodyInfo.findBestMatching(fixedValue);
        if (bodyInfo != null && bodyInfo.name().startsWith("ATMOSPHERE_TYPE_")) {
            fixedValue = bodyInfo.getName().toUpperCase();
        }
        return fixedValue.replaceAll("\\s", "");
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        BodyInfo bodyInfo = BodyInfo.findBestMatching(fixedValue);

        return bodyInfo != null && bodyInfo.name().startsWith("ATMOSPHERE_TYPE_");
    }

}
