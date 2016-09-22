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
public class GravityFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(GravityFixer.class);

    private static final NumberFormat NF = new DecimalFormat("0.00G", new DecimalFormatSymbols(Locale.US));
    private final Body eddbBody;

    public GravityFixer(Body eddbBody) {
        this.eddbBody = eddbBody;
    }

    @Override
    public String fixValue(String scannedText) {
        if (this.eddbBody != null && this.eddbBody.getGravity() != null) {
            return NF.format(this.eddbBody.getGravity());
        } else if (ONLY_FIX_WITH_EDDB_DATA) {
            return scannedText; // Do not try to fix
        } else {
            String fixedValue = scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(",", ".");
            if (fixedValue.contains(".") && fixedValue.indexOf(".") == fixedValue.length() - 4) {
                fixedValue = fixedValue.substring(0, fixedValue.length() - 1) + "G";
            }
            return fixedValue;
        }
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\d\\.\\d{2}G"); // min 0.00G, max 9.99G, always two decimal places
    }

}
