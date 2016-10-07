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
public class SurfaceTempFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(SurfaceTempFixer.class);

    private static final NumberFormat NF = new DecimalFormat("#,##0K", new DecimalFormatSymbols(Locale.US));
    private final Body eddbBody;

    public SurfaceTempFixer(Body eddbBody) {
        this.eddbBody = eddbBody;
    }

    @Override
    public String fixValue(String scannedText) {
        if (TRUST_EDDB && this.eddbBody != null && this.eddbBody.getSurface_temperature() != null) {
            return NF.format(this.eddbBody.getSurface_temperature());
        } else {
            String fixedValue = scannedText.toUpperCase().replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8");
            if (fixedValue.contains(",") && fixedValue.indexOf(",") == fixedValue.length() - 5) {
                fixedValue = fixedValue.substring(0, fixedValue.length() - 1) + "K";
            }
            return fixedValue;
        }
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("(\\d,)?\\d{1,3}(\\.\\d{2})?K"); // One to three digits followed by 'K', or 4 digits with thousands separator, min 0K, max 9,999K, optional 2 decimal places
    }

}
