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
public class SemiMajorAxisFixer implements ValueFixer {

    static final Logger logger = LogManager.getLogger(SemiMajorAxisFixer.class);

    private static final NumberFormat NF = new DecimalFormat("0.00AU", new DecimalFormatSymbols(Locale.US));
    private final Body eddbBody;

    public SemiMajorAxisFixer(Body eddbBody) {
        this.eddbBody = eddbBody;
    }

    @Override
    public String fixValue(String scannedText) {
        if (TRUST_EDDB && this.eddbBody != null && this.eddbBody.getSemi_major_axis() != null) {
            return NF.format(this.eddbBody.getSemi_major_axis());
        } else {
            String fixedValue = scannedText.toUpperCase().replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8").replace(",", ".");
            if (fixedValue.contains(".") && fixedValue.indexOf(".") == fixedValue.length() - 5) {
                fixedValue = fixedValue.substring(0, fixedValue.length() - 2) + "AU";
            }
            return fixedValue;
        }
    }

    @Override
    public boolean seemsPlausible(String fixedValue) {
        return fixedValue.matches("\\d{1,2}\\.\\d{2}AU"); // min 0.00AU, max 99.99AU, always two decimal places
    }

}