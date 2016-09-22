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
public class OrbitalPeriodFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(OrbitalPeriodFixer.class);

    private static final NumberFormat NF = new DecimalFormat("#,##0.0D", new DecimalFormatSymbols(Locale.US));
    private final Body eddbBody;

    public OrbitalPeriodFixer(Body eddbBody) {
        this.eddbBody = eddbBody;
    }

    @Override
    public String fixValue(String scannedText) {
        if (this.eddbBody != null && this.eddbBody.getOrbital_period() != null) {
            return NF.format(this.eddbBody.getOrbital_period());
        } else if (ONLY_FIX_WITH_EDDB_DATA) {
            return scannedText; // Do not try to fix
        } else {
            String fixedValue = scannedText.toUpperCase().replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8");
            if (fixedValue.contains(".") && fixedValue.indexOf(".") == fixedValue.length() - 3) {
                fixedValue = fixedValue.substring(0, fixedValue.length() - 1) + "D";
            }
            return fixedValue;
        }
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\d{1,3}\\.\\dD") || fixedValue.matches("\\d{1,2},\\d{3}\\.\\dD"); // min 0.0D, max 99,999.9D, always one decimal place
    }

}
