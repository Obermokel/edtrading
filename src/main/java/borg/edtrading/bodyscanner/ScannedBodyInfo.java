package borg.edtrading.bodyscanner;

import borg.edtrading.data.BodyInfo;
import borg.edtrading.data.Item;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;

/**
 * ScannedBodyInfo
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScannedBodyInfo {

    static final Logger logger = LogManager.getLogger(ScannedBodyInfo.class);

    private String screenshotFilename = null;
    private String systemName = null;
    private String bodyName = null;
    private BodyInfo bodyGroup = null; // Star, Planet, Belt
    // * Star
    // !no distance if arrival!
    private BodyInfo starType = null;
    private BigDecimal ageMillionYears = null;
    private BigDecimal solarMasses = null;
    private BigDecimal solarRadius = null;
    private BodyInfo bodyType = null; // Rocky, Icy, HMC, ...
    private BodyInfo terraforming = null;
    private BigDecimal distanceLs = null;
    private BodyInfo systemReserves = null;
    private BigDecimal earthMasses = null;
    private BigDecimal radiusKm = null;
    private BigDecimal gravityG = null;
    private BigDecimal surfaceTempK = null;
    private BigDecimal surfacePressureAtmospheres = null;
    private BodyInfo volcanism = null;
    private BodyInfo atmosphereType = null;
    private LinkedHashMap<BodyInfo, BigDecimal> atmosphere = null;
    private LinkedHashMap<BodyInfo, BigDecimal> composition = null;
    private BigDecimal orbitalPeriodD = null;
    private BigDecimal semiMajorAxisAU = null;
    private BigDecimal orbitalEccentricity = null;
    private BigDecimal orbitalInclinationDeg = null;
    private BigDecimal argOfPeriapsisDeg = null;
    private BigDecimal rotationalPeriodD = null;
    private Boolean tidallyLocked = null;
    private BigDecimal axialTiltDeg = null;
    private LinkedHashMap<Item, BigDecimal> planetMaterials = null;
    private List<ScannedRingInfo> rings = null;
    // * Belt
    // !no distance if arrival!
    private BodyInfo ringType = null;
    private BigDecimal moonMasses = null;

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        // screenshotFilename
        sb.append("==== ").append(this.getScreenshotFilename()).append(" ====").append("\n");
        // systemName // bodyName // distanceLs
        sb.append(this.getSystemName()).append(" // ").append(this.getBodyName());
        if (this.getDistanceLs() != null) {
            sb.append(" // ").append(String.format(Locale.US, "%.2fLS", this.getDistanceLs())).append("\n");
        } else {
            sb.append(" // ").append("ARRIVAL STAR").append("\n");
        }
        // bodyGroup|bodyType
        if (this.getBodyType() != null) {
            sb.append(this.getBodyType().getName()).append("\n");
        } else if (this.getStarType() != null) {
            sb.append(this.getStarType().getName()).append("\n");
        } else if (this.getBodyGroup() != null) {
            sb.append(this.getBodyGroup().getName()).append("\n");
        } else {
            sb.append("UNKNOWN BODY GROUP/TYPE").append("\n");
        }
        // terraforming
        if (this.getTerraforming() != null) {
            sb.append(this.getTerraforming().getName()).append("\n");
        }
        // systemReserves
        if (this.getSystemReserves() != null) {
            sb.append(this.getSystemReserves().getName()).append("\n");
        }

        // ---- STARS ----
        if (this.getBodyGroup() == BodyInfo.GROUP_STAR) {
            sb.append(String.format(Locale.US, "%-21s\t%.0f MILLION YEARS", "AGE:", this.getAgeMillionYears())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "SOLAR MASSES:", this.getSolarMasses())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "SOLAR RADIUS:", this.getSolarRadius())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.0fK", "SURFACE TEMP:", this.getSurfaceTempK())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.1fD", "ORBITAL PERIOD:", this.getOrbitalPeriodD())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2fAU", "SEMI MAJOR AXIS:", this.getSemiMajorAxisAU())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "ORBITAL ECCENTRICITY:", this.getOrbitalEccentricity())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2f°", "ORBITAL INCLINATION:", this.getOrbitalInclinationDeg())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2f°", "ARG OF PERIAPSIS:", this.getArgOfPeriapsisDeg())).append("\n");
        }

        // ---- BELTS ----
        if (this.getBodyGroup() == BodyInfo.GROUP_BELT) {
            sb.append(String.format(Locale.US, "%-21s\t%s", "RING TYPE:", this.getRingType() == null ? null : this.getRingType().getName())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "MOON MASSES:", this.getMoonMasses())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.1fD", "ORBITAL PERIOD:", this.getOrbitalPeriodD())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2fAU", "SEMI MAJOR AXIS:", this.getSemiMajorAxisAU())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "ORBITAL ECCENTRICITY:", this.getOrbitalEccentricity())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2f°", "ORBITAL INCLINATION:", this.getOrbitalInclinationDeg())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2f°", "ARG OF PERIAPSIS:", this.getArgOfPeriapsisDeg())).append("\n");
        }

        // ---- PLANETS ----
        if (this.getBodyGroup() == BodyInfo.GROUP_PLANET) {
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "EARTH MASSES:", this.getEarthMasses())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.0fKM", "RADIUS:", this.getRadiusKm())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2fG", "GRAVITY:", this.getGravityG())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.0fK", "SURFACE TEMP:", this.getSurfaceTempK())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2f", "SURFACE PRESSURE:", this.getSurfacePressureAtmospheres())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%s", "VOLCANISM:", this.getVolcanism() == null ? null : this.getVolcanism().getName())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%s", "ATMOSPHERE TYPE:", this.getAtmosphereType() == null ? null : this.getAtmosphereType().getName())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t", "ATMOSPHERE:"));
            if (this.getAtmosphere() == null) {
                sb.append("null").append("\n");
            } else {
                Iterator<BodyInfo> it = this.getAtmosphere().keySet().iterator();
                while (it.hasNext()) {
                    BodyInfo material = it.next();
                    BigDecimal percent = this.getAtmosphere().get(material);
                    sb.append(String.format(Locale.US, "%.1f%% %s", percent, material.getName())).append(it.hasNext() ? ", " : "\n");
                }
            }
            sb.append(String.format(Locale.US, "%-21s\t", "COMPOSITION:"));
            if (this.getComposition() == null) {
                sb.append("null").append("\n");
            } else {
                Iterator<BodyInfo> it = this.getComposition().keySet().iterator();
                while (it.hasNext()) {
                    BodyInfo material = it.next();
                    BigDecimal percent = this.getComposition().get(material);
                    sb.append(String.format(Locale.US, "%.1f%% %s", percent, material.getName())).append(it.hasNext() ? ", " : "\n");
                }
            }
            sb.append(String.format(Locale.US, "%-21s\t%.1fD", "ORBITAL PERIOD:", this.getOrbitalPeriodD())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2fAU", "SEMI MAJOR AXIS:", this.getSemiMajorAxisAU())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "ORBITAL ECCENTRICITY:", this.getOrbitalEccentricity())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2f°", "ORBITAL INCLINATION:", this.getOrbitalInclinationDeg())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.2f°", "ARG OF PERIAPSIS:", this.getArgOfPeriapsisDeg())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t%.1fD", "ROTATIONAL PERIOD:", this.getRotationalPeriodD()));
            if (Boolean.TRUE.equals(this.getTidallyLocked())) {
                sb.append(" (TIDALLY LOCKED)").append("\n");
            } else if (Boolean.FALSE.equals(this.getTidallyLocked())) {
                sb.append("\n");
            } else {
                sb.append(" (null)").append("\n");
            }
            sb.append(String.format(Locale.US, "%-21s\t%.2f°", "AXIAL TILT:", this.getAxialTiltDeg() == null ? null : this.getAxialTiltDeg().doubleValue())).append("\n");
            sb.append(String.format(Locale.US, "%-21s\t", "PLANET MATERIALS:"));
            if (this.getPlanetMaterials() == null) {
                sb.append("null").append("\n");
            } else {
                Iterator<Item> it = this.getPlanetMaterials().keySet().iterator();
                while (it.hasNext()) {
                    Item material = it.next();
                    BigDecimal percent = this.getPlanetMaterials().get(material);
                    sb.append(String.format(Locale.US, "%s(%.1f%%)", material.getName(), percent)).append(it.hasNext() ? ", " : "\n");
                }
            }
        }

        // ---- RINGS ----
        if (this.getRings() != null) {
            for (ScannedRingInfo sri : this.getRings()) {
                sb.append(sri.toString()).append("\n");
            }
        }

        return sb.toString().trim();
    }

    public ScannedBodyInfo(String screenshotFilename, String systemName) {
        this.setScreenshotFilename(screenshotFilename);
        this.setSystemName(systemName);
    }

    public String getScreenshotFilename() {
        return this.screenshotFilename;
    }

    public void setScreenshotFilename(String screenshotFilename) {
        this.screenshotFilename = screenshotFilename;
    }

    public String getSystemName() {
        return this.systemName;
    }

    public void setSystemName(String systemName) {
        this.systemName = systemName;
    }

    public String getBodyName() {
        return this.bodyName;
    }

    public void setBodyName(String bodyName) {
        this.bodyName = bodyName;
    }

    public BodyInfo getBodyGroup() {
        return this.bodyGroup;
    }

    public void setBodyGroup(BodyInfo bodyGroup) {
        this.bodyGroup = bodyGroup;
    }

    public BodyInfo getStarType() {
        return this.starType;
    }

    public void setStarType(BodyInfo starType) {
        this.starType = starType;
    }

    public BigDecimal getAgeMillionYears() {
        return this.ageMillionYears;
    }

    public void setAgeMillionYears(BigDecimal ageMillionYears) {
        this.ageMillionYears = ageMillionYears;
    }

    public BigDecimal getSolarMasses() {
        return this.solarMasses;
    }

    public void setSolarMasses(BigDecimal solarMasses) {
        this.solarMasses = solarMasses;
    }

    public BigDecimal getSolarRadius() {
        return this.solarRadius;
    }

    public void setSolarRadius(BigDecimal solarRadius) {
        this.solarRadius = solarRadius;
    }

    public BodyInfo getBodyType() {
        return this.bodyType;
    }

    public void setBodyType(BodyInfo bodyType) {
        this.bodyType = bodyType;
    }

    public BodyInfo getTerraforming() {
        return this.terraforming;
    }

    public void setTerraforming(BodyInfo terraforming) {
        this.terraforming = terraforming;
    }

    public BigDecimal getDistanceLs() {
        return this.distanceLs;
    }

    public void setDistanceLs(BigDecimal distanceLs) {
        this.distanceLs = distanceLs;
    }

    public BodyInfo getSystemReserves() {
        return this.systemReserves;
    }

    public void setSystemReserves(BodyInfo systemReserves) {
        this.systemReserves = systemReserves;
    }

    public BigDecimal getEarthMasses() {
        return this.earthMasses;
    }

    public void setEarthMasses(BigDecimal earthMasses) {
        this.earthMasses = earthMasses;
    }

    public BigDecimal getRadiusKm() {
        return this.radiusKm;
    }

    public void setRadiusKm(BigDecimal radiusKm) {
        this.radiusKm = radiusKm;
    }

    public BigDecimal getGravityG() {
        return this.gravityG;
    }

    public void setGravityG(BigDecimal gravityG) {
        this.gravityG = gravityG;
    }

    public BigDecimal getSurfaceTempK() {
        return this.surfaceTempK;
    }

    public void setSurfaceTempK(BigDecimal surfaceTempK) {
        this.surfaceTempK = surfaceTempK;
    }

    public BigDecimal getSurfacePressureAtmospheres() {
        return this.surfacePressureAtmospheres;
    }

    public void setSurfacePressureAtmospheres(BigDecimal surfacePressureAtmospheres) {
        this.surfacePressureAtmospheres = surfacePressureAtmospheres;
    }

    public BodyInfo getVolcanism() {
        return this.volcanism;
    }

    public void setVolcanism(BodyInfo volcanism) {
        this.volcanism = volcanism;
    }

    public BodyInfo getAtmosphereType() {
        return this.atmosphereType;
    }

    public void setAtmosphereType(BodyInfo atmosphereType) {
        this.atmosphereType = atmosphereType;
    }

    public LinkedHashMap<BodyInfo, BigDecimal> getAtmosphere() {
        return this.atmosphere;
    }

    public void setAtmosphere(LinkedHashMap<BodyInfo, BigDecimal> atmosphere) {
        this.atmosphere = atmosphere;
    }

    public LinkedHashMap<BodyInfo, BigDecimal> getComposition() {
        return this.composition;
    }

    public void setComposition(LinkedHashMap<BodyInfo, BigDecimal> composition) {
        this.composition = composition;
    }

    public BigDecimal getOrbitalPeriodD() {
        return this.orbitalPeriodD;
    }

    public void setOrbitalPeriodD(BigDecimal orbitalPeriodD) {
        this.orbitalPeriodD = orbitalPeriodD;
    }

    public BigDecimal getSemiMajorAxisAU() {
        return this.semiMajorAxisAU;
    }

    public void setSemiMajorAxisAU(BigDecimal semiMajorAxisAU) {
        this.semiMajorAxisAU = semiMajorAxisAU;
    }

    public BigDecimal getOrbitalEccentricity() {
        return this.orbitalEccentricity;
    }

    public void setOrbitalEccentricity(BigDecimal orbitalEccentricity) {
        this.orbitalEccentricity = orbitalEccentricity;
    }

    public BigDecimal getOrbitalInclinationDeg() {
        return this.orbitalInclinationDeg;
    }

    public void setOrbitalInclinationDeg(BigDecimal orbitalInclinationDeg) {
        this.orbitalInclinationDeg = orbitalInclinationDeg;
    }

    public BigDecimal getArgOfPeriapsisDeg() {
        return this.argOfPeriapsisDeg;
    }

    public void setArgOfPeriapsisDeg(BigDecimal argOfPeriapsisDeg) {
        this.argOfPeriapsisDeg = argOfPeriapsisDeg;
    }

    public BigDecimal getRotationalPeriodD() {
        return this.rotationalPeriodD;
    }

    public void setRotationalPeriodD(BigDecimal rotationalPeriodD) {
        this.rotationalPeriodD = rotationalPeriodD;
    }

    public Boolean getTidallyLocked() {
        return this.tidallyLocked;
    }

    public void setTidallyLocked(Boolean tidallyLocked) {
        this.tidallyLocked = tidallyLocked;
    }

    public BigDecimal getAxialTiltDeg() {
        return this.axialTiltDeg;
    }

    public void setAxialTiltDeg(BigDecimal axialTiltDeg) {
        this.axialTiltDeg = axialTiltDeg;
    }

    public LinkedHashMap<Item, BigDecimal> getPlanetMaterials() {
        return this.planetMaterials;
    }

    public void setPlanetMaterials(LinkedHashMap<Item, BigDecimal> planetMaterials) {
        this.planetMaterials = planetMaterials;
    }

    public List<ScannedRingInfo> getRings() {
        return this.rings;
    }

    public void setRings(List<ScannedRingInfo> rings) {
        this.rings = rings;
    }

    public BodyInfo getRingType() {
        return this.ringType;
    }

    public void setRingType(BodyInfo ringType) {
        this.ringType = ringType;
    }

    public BigDecimal getMoonMasses() {
        return this.moonMasses;
    }

    public void setMoonMasses(BigDecimal moonMasses) {
        this.moonMasses = moonMasses;
    }

}
