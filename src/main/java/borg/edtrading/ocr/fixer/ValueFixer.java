package borg.edtrading.ocr.fixer;

/**
 * ValueFixer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface ValueFixer {

    /**
     * Must return the correct value, including case, but w/o whitespaces.
     * <br>
     * Correct examples: 7,733KM (radius), 1.55G (gravity), 113.8D (orbital period)
     * <br>
     * Wrong examples: 7,733km (case), 1,55G (punctuation), 113.8 D (whitespace)
     */
    String fixValue(String scannedText);

    /**
     * Should always check for the correct number of decimal places. But can also include
     * more sophisticated checks like >359°, >10G gravity, <0K temp.
     */
    boolean seemsPlausible(String fixedValue);

}
