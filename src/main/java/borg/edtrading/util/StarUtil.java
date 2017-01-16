package borg.edtrading.util;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;

/**
 * StarUtil
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public abstract class StarUtil {

    static final Logger logger = LogManager.getLogger(StarUtil.class);

    public static Color spectralClassToColor(String spectralClass) {
        if (spectralClass == null || spectralClass.isEmpty()) {
            return new Color(80, 80, 80);
        } else if ("O".equals(spectralClass)) {
            return new Color(207, 218, 235);
        } else if ("B".equals(spectralClass)) {
            return new Color(200, 219, 227);
        } else if ("A".equals(spectralClass)) {
            return new Color(202, 201, 205);
        } else if ("F".equals(spectralClass)) {
            return new Color(210, 196, 144);
        } else if ("G".equals(spectralClass)) {
            return new Color(233, 194, 125);
        } else if ("K".equals(spectralClass) || "K_RedGiant".equals(spectralClass) || "K_OrangeGiant".equals(spectralClass)) {
            return new Color(242, 152, 79);
        } else if ("M".equals(spectralClass) || "M_RedGiant".equals(spectralClass) || "M_OrangeGiant".equals(spectralClass)) {
            return new Color(221, 125, 61);
        } else if ("Y".equals(spectralClass)) {
            return new Color(58, 18, 20);
        } else if ("L".equals(spectralClass)) {
            return new Color(144, 14, 45);
        } else if ("T".equals(spectralClass)) {
            return new Color(72, 8, 41);
        } else if ("TTS".equals(spectralClass)) {
            return new Color(239, 217, 90);
        } else if ("N".equals(spectralClass) || "NS".equals(spectralClass)) {
            return new Color(224, 224, 255);
        } else if ("H".equals(spectralClass) || "BH".equals(spectralClass) || "SMBH".equals(spectralClass)) {
            return new Color(0, 0, 0);
        } else if (spectralClass.startsWith("D")) {
            return new Color(224, 212, 224); // White dwarf
        } else {
            logger.warn("Unknown spectral class '" + spectralClass + "'");
            return new Color(255, 0, 255);
        }
    }

}
