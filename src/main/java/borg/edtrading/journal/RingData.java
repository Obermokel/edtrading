package borg.edtrading.journal;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * RingData
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RingData implements Serializable {

    private static final long serialVersionUID = 9133617747074981132L;

    private final String name;
    private final String ringClass;
    private final BigDecimal massMT;
    private final BigDecimal innerRad;
    private final BigDecimal outerRad;

    public RingData(String name, String ringClass, BigDecimal massMT, BigDecimal innerRad, BigDecimal outerRad) {
        this.name = name;
        this.ringClass = ringClass;
        this.massMT = massMT;
        this.innerRad = innerRad;
        this.outerRad = outerRad;
    }

    public String getName() {
        return this.name;
    }

    public String getRingClass() {
        return this.ringClass;
    }

    public BigDecimal getMassMT() {
        return this.massMT;
    }

    public BigDecimal getInnerRad() {
        return this.innerRad;
    }

    public BigDecimal getOuterRad() {
        return this.outerRad;
    }

}
