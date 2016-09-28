package borg.edtrading.ocr.fixer;

import borg.edtrading.data.Body;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.Locale;

/**
 * EarthMassesFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SurfacePressureFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(SurfacePressureFixer.class);

    private static final NumberFormat NF = new DecimalFormat("0.00ATMOSPHERES", new DecimalFormatSymbols(Locale.US));
    private final Body eddbBody;

    public SurfacePressureFixer(Body eddbBody) {
        this.eddbBody = eddbBody;
    }

    @Override
    public String fixValue(String scannedText) {
        if (TRUST_EDDB && this.eddbBody != null && this.eddbBody.getSurface_pressure() != null) {
            return NF.format(this.eddbBody.getSurface_pressure());
        } else {
            if (scannedText.length() <= "ATMOSPHERES".length()) {
                return scannedText;
            } else {
                String fixedValue = scannedText.substring(0, scannedText.length() - "ATMOSPHERES".length());
                return fixedValue.toUpperCase().replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(",", ".") + "ATMOSPHERES";
            }
        }
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\d{1,2}\\.\\d{2}ATMOSPHERES"); // Unit is ATMOSPHERES, 2 decimal places, max 99.99
    }

}
