package borg.edtrading.journal.entries.exploration;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.RingData;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * ScanEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScanEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 2391971591810270978L;

    private final String bodyName;
    private final Float distanceFromArrivalLS;
    private final String starType;
    private final Float stellarMass;
    private final Float radius;
    private final Float absoluteMagnitude;
    private final Integer ageMY;
    private final Float surfaceTemperature;
    private final Float semiMajorAxis;
    private final Float eccentricity;
    private final Float orbitalInclination;
    private final Float periapsis;
    private final Float orbitalPeriod;
    private final Float rotationPeriod;
    private final List<RingData> rings;
    private final Boolean tidalLock;
    private final String terraformState;
    private final String planetClass;
    private final Float massEM;
    private final Float surfaceGravity;
    private final Float surfacePressure;
    private final Boolean landable;
    private final String atmosphere;
    private final String atmosphereType;
    private final String volcanism;
    private final Map<String, Float> materials;

    public ScanEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.bodyName = this.readString(data, "BodyName");
        this.distanceFromArrivalLS = this.readFloat(data, "DistanceFromArrivalLS");
        this.starType = this.readString(data, "StarType");
        this.stellarMass = this.readFloat(data, "StellarMass");
        this.radius = this.readFloat(data, "Radius");
        this.absoluteMagnitude = this.readFloat(data, "AbsoluteMagnitude");
        this.ageMY = this.readInt(data, "Age_MY");
        this.surfaceTemperature = this.readFloat(data, "SurfaceTemperature");
        this.semiMajorAxis = this.readFloat(data, "SemiMajorAxis");
        this.eccentricity = this.readFloat(data, "Eccentricity");
        this.orbitalInclination = this.readFloat(data, "OrbitalInclination");
        this.periapsis = this.readFloat(data, "Periapsis");
        this.orbitalPeriod = this.readFloat(data, "OrbitalPeriod");
        this.rotationPeriod = this.readFloat(data, "RotationPeriod");
        this.rings = this.readRings(data, "Rings");
        this.tidalLock = this.readBoolean(data, "TidalLock");
        this.terraformState = this.readString(data, "TerraformState");
        this.planetClass = this.readString(data, "PlanetClass");
        this.massEM = this.readFloat(data, "MassEM");
        this.surfaceGravity = this.readFloat(data, "SurfaceGravity");
        this.surfacePressure = this.readFloat(data, "SurfacePressure");
        this.landable = this.readBoolean(data, "Landable");
        this.atmosphere = this.readString(data, "Atmosphere");
        this.atmosphereType = this.readString(data, "AtmosphereType");
        this.volcanism = this.readString(data, "Volcanism");
        this.materials = this.readPercentages(data, "Materials");
    }

    public String getBodyName() {
        return this.bodyName;
    }

    public Float getDistanceFromArrivalLS() {
        return this.distanceFromArrivalLS;
    }

    public String getStarType() {
        return this.starType;
    }

    public Float getStellarMass() {
        return this.stellarMass;
    }

    public Float getRadius() {
        return this.radius;
    }

    public Float getAbsoluteMagnitude() {
        return this.absoluteMagnitude;
    }

    public Integer getAgeMY() {
        return this.ageMY;
    }

    public Float getSurfaceTemperature() {
        return this.surfaceTemperature;
    }

    public Float getSemiMajorAxis() {
        return this.semiMajorAxis;
    }

    public Float getEccentricity() {
        return this.eccentricity;
    }

    public Float getOrbitalInclination() {
        return this.orbitalInclination;
    }

    public Float getPeriapsis() {
        return this.periapsis;
    }

    public Float getOrbitalPeriod() {
        return this.orbitalPeriod;
    }

    public Float getRotationPeriod() {
        return this.rotationPeriod;
    }

    public List<RingData> getRings() {
        return this.rings;
    }

    public Boolean getTidalLock() {
        return this.tidalLock;
    }

    public String getTerraformState() {
        return this.terraformState;
    }

    public String getPlanetClass() {
        return this.planetClass;
    }

    public Float getMassEM() {
        return this.massEM;
    }

    public Float getSurfaceGravity() {
        return this.surfaceGravity;
    }

    public Float getSurfacePressure() {
        return this.surfacePressure;
    }

    public Boolean getLandable() {
        return this.landable;
    }

    public String getAtmosphere() {
        return this.atmosphere;
    }

    public String getAtmosphereType() {
        return this.atmosphereType;
    }

    public String getVolcanism() {
        return this.volcanism;
    }

    public Map<String, Float> getMaterials() {
        return this.materials;
    }

}
