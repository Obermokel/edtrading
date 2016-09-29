package borg.edtrading.data;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;
import java.util.Locale;

/**
 * ScannedRingInfo
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScannedRingInfo {

    static final Logger logger = LogManager.getLogger(ScannedRingInfo.class);

    private String ringName = null;
    private BodyInfo ringType = null;
    private BigDecimal massMt = null;
    private BigDecimal semiMajorAxisAU = null;
    private BigDecimal innerRadiusKm = null;
    private BigDecimal outerRadiusKm = null;

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getRingName()).append(" // ");
        sb.append(this.getRingType() == null ? null : this.getRingType().getName()).append(" // ");
        sb.append(String.format(Locale.US, "%.1fMT", this.getMassMt())).append(" // ");
        sb.append(String.format(Locale.US, "%.0fKM to %.0fKM", this.getInnerRadiusKm(), this.getOuterRadiusKm()));
        return sb.toString();
    }

    public String getRingName() {
        return this.ringName;
    }

    public void setRingName(String ringName) {
        this.ringName = ringName;
    }

    public BodyInfo getRingType() {
        return this.ringType;
    }

    public void setRingType(BodyInfo ringType) {
        this.ringType = ringType;
    }

    public BigDecimal getMassMt() {
        return this.massMt;
    }

    public void setMassMt(BigDecimal massMt) {
        this.massMt = massMt;
    }

    public BigDecimal getSemiMajorAxisAU() {
        return this.semiMajorAxisAU;
    }

    public void setSemiMajorAxisAU(BigDecimal semiMajorAxisAU) {
        this.semiMajorAxisAU = semiMajorAxisAU;
    }

    public BigDecimal getInnerRadiusKm() {
        return this.innerRadiusKm;
    }

    public void setInnerRadiusKm(BigDecimal innerRadiusKm) {
        this.innerRadiusKm = innerRadiusKm;
    }

    public BigDecimal getOuterRadiusKm() {
        return this.outerRadiusKm;
    }

    public void setOuterRadiusKm(BigDecimal outerRadiusKm) {
        this.outerRadiusKm = outerRadiusKm;
    }

}
