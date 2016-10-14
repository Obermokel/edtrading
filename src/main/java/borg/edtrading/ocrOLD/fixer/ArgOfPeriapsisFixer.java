package borg.edtrading.ocrOLD.fixer;

import borg.edtrading.data.Body;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.Locale;

/**
 * RadiusFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ArgOfPeriapsisFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(ArgOfPeriapsisFixer.class);

    private static final BigDecimal MIN_VALUE = new BigDecimal("0.0");
    private static final BigDecimal MAX_VALUE = new BigDecimal("360.00");
    private static final NumberFormat NF = new DecimalFormat("0.00", new DecimalFormatSymbols(Locale.US));
    private final Body eddbBody;

    public ArgOfPeriapsisFixer(Body eddbBody) {
        this.eddbBody = eddbBody;
    }

    @Override
    public String fixValue(String scannedText) {
        if (TRUST_EDDB && this.eddbBody != null && this.eddbBody.getArg_of_periapsis() != null) {
            return NF.format(this.eddbBody.getArg_of_periapsis()) + (scannedText.contains("°") ? "°" : "");
        } else {
            return scannedText.toUpperCase().replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(",", ".").replaceAll("\\.?°\\.?", "°");
        }
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        // min 0.00°, max 359.99°, always two decimal places, ° optional
        if (!fixedValue.matches("\\d{1,3}\\.\\d{2}°?")) {
            return false;
        } else {
            BigDecimal actualValue = new BigDecimal(fixedValue.replace("°", ""));

            return actualValue.compareTo(MIN_VALUE) >= 0 && actualValue.compareTo(MAX_VALUE) < 0;
        }
    }

}
