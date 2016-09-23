package borg.edtrading.ocr.fixer;

import borg.edtrading.data.Body;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.Locale;

/**
 * RadiusFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class AxialTiltFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(AxialTiltFixer.class);

    private static final NumberFormat NF = new DecimalFormat("0.00", new DecimalFormatSymbols(Locale.US));
    private final Body eddbBody;

    public AxialTiltFixer(Body eddbBody) {
        this.eddbBody = eddbBody;
    }

    @Override
    public String fixValue(String scannedText) {
        if (TRUST_EDDB && this.eddbBody != null && this.eddbBody.getAxis_tilt() != null) {
            return NF.format(this.eddbBody.getAxis_tilt()) + (scannedText.contains("°") ? "°" : "");
        } else {
            return scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(",", ".").replaceAll("\\.?°\\.?", "°");
        }
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\-?\\d{1,3}\\.\\d{2}°?"); // min -999.99°, max 999.99°, always two decimal places, ° optional
    }

}
