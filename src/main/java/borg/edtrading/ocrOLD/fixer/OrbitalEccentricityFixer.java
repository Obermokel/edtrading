package borg.edtrading.ocrOLD.fixer;

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
public class OrbitalEccentricityFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(OrbitalEccentricityFixer.class);

    private static final NumberFormat NF = new DecimalFormat("0.0000", new DecimalFormatSymbols(Locale.US));
    private final Body eddbBody;

    public OrbitalEccentricityFixer(Body eddbBody) {
        this.eddbBody = eddbBody;
    }

    @Override
    public String fixValue(String scannedText) {
        if (TRUST_EDDB && this.eddbBody != null && this.eddbBody.getOrbital_eccentricity() != null) {
            return NF.format(this.eddbBody.getOrbital_eccentricity());
        } else {
            return scannedText.toUpperCase().replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(",", ".");
        }
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\d\\.\\d{4}"); // No unit, 4 decimal places, max 9.999
    }

}