package borg.edtrading.ocr.fixer;

import borg.edtrading.templatematching.Match;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;
import java.util.List;

/**
 * AbstractBigDecimalWithOptionalUnitFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public abstract class AbstractBigDecimalWithOptionalUnitFixer extends AbstractFixer {

    static final Logger logger = LogManager.getLogger(AbstractBigDecimalWithOptionalUnitFixer.class);

    @Override
    public final Object fix(List<Match> matches) throws FixerException {
        final String scannedText = this.matchesToText(matches);

        String fixedText = scannedText;

        // digits->letters for unit matching
        fixedText = fixedText.replace("0", "O");
        fixedText = fixedText.replace("0", "D");
        fixedText = fixedText.replace("1", "I");
        fixedText = fixedText.replace("5", "S");
        fixedText = fixedText.replace("8", "B");

        if (this.hasUnit() && !fixedText.contains(this.getUnit())) {
            throw new FixerException("Fixed text '" + fixedText + "' does not contain '" + this.getUnit() + "' - Scanned text was '" + scannedText + "'");
        } else {
            fixedText = !this.hasUnit() ? fixedText : fixedText.substring(0, fixedText.lastIndexOf(this.getUnit())); // Remove the unit and everything after it

            // letters->digits for value matching
            fixedText = fixedText.replace("l", "1");
            fixedText = fixedText.replace("o", "0");
            fixedText = fixedText.replace("s", "5");
            fixedText = fixedText.replace("B", "8");
            fixedText = fixedText.replace("D", "0");
            fixedText = fixedText.replace("I", "1");
            fixedText = fixedText.replace("O", "0");
            fixedText = fixedText.replace("S", "5");

            // Remove everything that is not a digit, separator or sign
            fixedText = fixedText.replaceAll("[^0-9,\\.\\-]", "");

            // Remove crap at the start until the remaining text starts with a sign or digit
            int offsetInScannedText = 0;
            while (fixedText.length() > 0 && !fixedText.matches("^[0-9\\-].*")) {
                fixedText = fixedText.substring(1);
                offsetInScannedText++;
            }

            // Convert all separators to commas to make pattern matching easier
            fixedText = fixedText.replace(".", ",");

            if (!fixedText.matches(this.getPattern())) {
                throw new FixerException("Fixed text '" + fixedText + "' does not match <" + this.getPattern() + "> - Scanned text was '" + scannedText + "'");
            } else {
                // Convert last comma back to decimal separator
                if (this.patternNumDecimals() > 0) {
                    fixedText = fixedText.substring(0, fixedText.lastIndexOf(",")) + "." + fixedText.substring(fixedText.lastIndexOf(",") + 1);
                }

                // Remove commas for parsing
                final String parsableText = fixedText.replace(",", "");

                // Add unit back
                if (this.hasUnit()) {
                    fixedText = fixedText + this.getUnit();
                }

                try {
                    // Parse
                    BigDecimal value = new BigDecimal(parsableText);

                    // Fix
                    for (int i = 0; i < fixedText.length(); i++) {
                        matches.get(i + offsetInScannedText).setShouldHaveBeen(Character.toString(fixedText.charAt(i)));
                    }

                    // Return
                    return value;
                } catch (NumberFormatException e) {
                    throw new FixerException("Fixed text '" + fixedText + "' is not a BigDecimal - Scanned text was '" + scannedText + "'");
                }
            }
        }
    }

    protected abstract String getUnit();

    protected boolean hasUnit() {
        return StringUtils.isNotEmpty(this.getUnit());
    }

    protected String getPattern() {
        String pattern = "";
        if (this.patternHasSign()) {
            pattern += "\\-";
        }
        pattern += "\\d{1,3}";
        if (this.patternHasThousands()) {
            pattern += "(,\\d{3})*";
        }
        if (this.patternNumDecimals() > 0) {
            pattern += ",\\d{" + this.patternNumDecimals() + "}";
        }
        return pattern;
    }

    protected abstract boolean patternHasSign();

    protected abstract boolean patternHasThousands();

    protected abstract int patternNumDecimals();

}
