package borg.edtrading.ocrOLD.fixer;

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
public class RadiusFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(RadiusFixer.class);

    private static final NumberFormat NF = new DecimalFormat("#,##0KM", new DecimalFormatSymbols(Locale.US));
    private final Body eddbBody;

    public RadiusFixer(Body eddbBody) {
        this.eddbBody = eddbBody;
    }

    @Override
    public String fixValue(String scannedText) {
        if (TRUST_EDDB && this.eddbBody != null && this.eddbBody.getRadius() != null) {
            return NF.format(this.eddbBody.getRadius());
        } else {
            String fixedValue = scannedText.toUpperCase().replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(".", ",");
            if (fixedValue.contains(",") && fixedValue.indexOf(",") == fixedValue.length() - 6) {
                fixedValue = fixedValue.substring(0, fixedValue.length() - 2) + "KM";
            }
            return fixedValue;
        }
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("(\\d{1,3},)?\\d{3}KM"); // min 100KM, max 999,999KM
    }

}
