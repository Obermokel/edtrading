package borg.edtrading.eddb.data;

import com.google.gson.annotations.SerializedName;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * EddbBody
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
@Document(indexName = "eddb", type = "body", shards = 10, replicas = 0)
public class EddbBody implements EddbEntity {

    private static final long serialVersionUID = -785705194031217696L;

    public static final Long TYPE_ID_BLACK_HOLE = new Long(1);
    public static final Long TYPE_ID_SUPERMASSIVE_BLACK_HOLE = new Long(2);
    public static final Long TYPE_ID_NEUTRON_STAR = new Long(3);

    private transient EddbSystem system = null;

    public EddbSystem getSystem() {
        return this.system;
    }

    public void setSystem(EddbSystem system) {
        this.system = system;
    }

    @Id
    @SerializedName("id")
    private Long id = null;
    @Field(type = FieldType.Date)
    @SerializedName("created_at")
    private Date createdAt = null;
    @Field(type = FieldType.Date)
    @SerializedName("updated_at")
    private Date updatedAt = null;
    @SerializedName("name")
    private String name = null;
    @SerializedName("system_id")
    private Long systemId = null;
    @SerializedName("group_id")
    private Long groupId = null;
    @SerializedName("group_name")
    private String groupName = null;
    @SerializedName("type_id")
    private Long typeId = null;
    @SerializedName("type_name")
    private String typeName = null;
    @Field(type = FieldType.Double)
    @SerializedName("distance_to_arrival")
    private BigDecimal distanceToArrival = null;
    @SerializedName("full_spectral_class")
    private String fullSpectralClass = null;
    @SerializedName("spectral_class")
    private String spectralClass = null;
    @SerializedName("spectral_sub_class")
    private String spectralSubClass = null;
    @SerializedName("luminosity_class")
    private String luminosityClass = null;
    @SerializedName("luminosity_sub_class")
    private String luminositySubClass = null;
    @Field(type = FieldType.Double)
    @SerializedName("surface_temperature")
    private BigDecimal surfaceTemperature = null;
    @SerializedName("is_main_star")
    private Boolean isMainStar = null;
    @Field(type = FieldType.Double)
    @SerializedName("age")
    private BigDecimal age = null;
    @Field(type = FieldType.Double)
    @SerializedName("solar_masses")
    private BigDecimal solarMasses = null;
    @Field(type = FieldType.Double)
    @SerializedName("solar_radius")
    private BigDecimal solarRadius = null;
    @SerializedName("catalogue_gliese_id")
    private String catalogueGlieseId = null;
    @SerializedName("catalogue_hipp_id")
    private String catalogueHippId = null;
    @SerializedName("catalogue_hd_id")
    private String catalogueHdId = null;
    @SerializedName("volcanism_type_id")
    private Long volcanismTypeId = null;
    @SerializedName("volcanism_type_name")
    private String volcanismTypeName = null;
    @SerializedName("atmosphere_type_id")
    private Long atmosphereTypeId = null;
    @SerializedName("atmosphere_type_name")
    private String atmosphereTypeName = null;
    @SerializedName("terraforming_state_id")
    private Long terraformingStateId = null;
    @SerializedName("terraforming_state_name")
    private String terraformingStateName = null;
    @Field(type = FieldType.Double)
    @SerializedName("earth_masses")
    private BigDecimal earthMasses = null;
    @Field(type = FieldType.Double)
    @SerializedName("radius")
    private BigDecimal radius = null;
    @Field(type = FieldType.Double)
    @SerializedName("gravity")
    private BigDecimal gravity = null;
    @Field(type = FieldType.Double)
    @SerializedName("surface_pressure")
    private BigDecimal surfacePressure = null;
    @Field(type = FieldType.Double)
    @SerializedName("orbital_period")
    private BigDecimal orbitalPeriod = null;
    @Field(type = FieldType.Double)
    @SerializedName("semi_major_axis")
    private BigDecimal semiMajorAxis = null;
    @Field(type = FieldType.Double)
    @SerializedName("orbital_eccentricity")
    private BigDecimal orbitalEccentricity = null;
    @Field(type = FieldType.Double)
    @SerializedName("orbital_inclination")
    private BigDecimal orbitalInclination = null;
    @Field(type = FieldType.Double)
    @SerializedName("arg_of_periapsis")
    private BigDecimal argOfPeriapsis = null;
    @Field(type = FieldType.Double)
    @SerializedName("rotational_period")
    private BigDecimal rotationalPeriod = null;
    @SerializedName("is_rotational_period_tidally_locked")
    private Boolean isRotationalPeriodTidallyLocked = null;
    @Field(type = FieldType.Double)
    @SerializedName("axis_tilt")
    private BigDecimal axisTilt = null;
    @SerializedName("eg_id")
    private Long egId = null;
    @Field(type = FieldType.Double)
    @SerializedName("belt_moon_masses")
    private BigDecimal beltMoonMasses = null;
    @SerializedName("rings")
    private List<EddbRing> rings = null;
    @SerializedName("atmosphere_composition")
    private List<AtmosphereShare> atmosphereComposition = null;
    @SerializedName("solid_composition")
    private List<CompositionShare> solidComposition = null;
    @SerializedName("materials")
    private List<MaterialShare> materials = null;
    @SerializedName("is_landable")
    private Boolean isLandable = null;

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (this.getClass() != obj.getClass()) {
            return false;
        }
        EddbBody other = (EddbBody) obj;
        if (this.id == null) {
            if (other.id != null) {
                return false;
            }
        } else if (!this.id.equals(other.id)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return this.id.hashCode();
    }

    @Override
    public String toString() {
        return "#" + this.id + " " + this.name;
    }

    @Override
    public Long getId() {
        return this.id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Date getCreatedAt() {
        return this.createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }

    public Date getUpdatedAt() {
        return this.updatedAt;
    }

    public void setUpdatedAt(Date updatedAt) {
        this.updatedAt = updatedAt;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Long getSystemId() {
        return this.systemId;
    }

    public void setSystemId(Long systemId) {
        this.systemId = systemId;
    }

    public Long getGroupId() {
        return this.groupId;
    }

    public void setGroupId(Long groupId) {
        this.groupId = groupId;
    }

    public String getGroupName() {
        return this.groupName;
    }

    public void setGroupName(String groupName) {
        this.groupName = groupName == null ? null : groupName.intern();
    }

    public Long getTypeId() {
        return this.typeId;
    }

    public void setTypeId(Long typeId) {
        this.typeId = typeId;
    }

    public String getTypeName() {
        return this.typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName == null ? null : typeName.intern();
    }

    public BigDecimal getDistanceToArrival() {
        return this.distanceToArrival;
    }

    public void setDistanceToArrival(BigDecimal distanceToArrival) {
        this.distanceToArrival = distanceToArrival;
    }

    public String getFullSpectralClass() {
        return this.fullSpectralClass;
    }

    public void setFullSpectralClass(String fullSpectralClass) {
        this.fullSpectralClass = fullSpectralClass == null ? null : fullSpectralClass.intern();
    }

    public String getSpectralClass() {
        return this.spectralClass;
    }

    public void setSpectralClass(String spectralClass) {
        this.spectralClass = spectralClass == null ? null : spectralClass.intern();
    }

    public String getSpectralSubClass() {
        return this.spectralSubClass;
    }

    public void setSpectralSubClass(String spectralSubClass) {
        this.spectralSubClass = spectralSubClass == null ? null : spectralSubClass.intern();
    }

    public String getLuminosityClass() {
        return this.luminosityClass;
    }

    public void setLuminosityClass(String luminosityClass) {
        this.luminosityClass = luminosityClass == null ? null : luminosityClass.intern();
    }

    public String getLuminositySubClass() {
        return this.luminositySubClass;
    }

    public void setLuminositySubClass(String luminositySubClass) {
        this.luminositySubClass = luminositySubClass == null ? null : luminositySubClass.intern();
    }

    public BigDecimal getSurfaceTemperature() {
        return this.surfaceTemperature;
    }

    public void setSurfaceTemperature(BigDecimal surfaceTemperature) {
        this.surfaceTemperature = surfaceTemperature;
    }

    public Boolean getIsMainStar() {
        return this.isMainStar;
    }

    public void setIsMainStar(Boolean isMainStar) {
        this.isMainStar = isMainStar;
    }

    public BigDecimal getAge() {
        return this.age;
    }

    public void setAge(BigDecimal age) {
        this.age = age;
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

    public String getCatalogueGlieseId() {
        return this.catalogueGlieseId;
    }

    public void setCatalogueGlieseId(String catalogueGlieseId) {
        this.catalogueGlieseId = catalogueGlieseId;
    }

    public String getCatalogueHippId() {
        return this.catalogueHippId;
    }

    public void setCatalogueHippId(String catalogueHippId) {
        this.catalogueHippId = catalogueHippId;
    }

    public String getCatalogueHdId() {
        return this.catalogueHdId;
    }

    public void setCatalogueHdId(String catalogueHdId) {
        this.catalogueHdId = catalogueHdId;
    }

    public Long getVolcanismTypeId() {
        return this.volcanismTypeId;
    }

    public void setVolcanismTypeId(Long volcanismTypeId) {
        this.volcanismTypeId = volcanismTypeId;
    }

    public String getVolcanismTypeName() {
        return this.volcanismTypeName;
    }

    public void setVolcanismTypeName(String volcanismTypeName) {
        this.volcanismTypeName = volcanismTypeName == null ? null : volcanismTypeName.intern();
    }

    public Long getAtmosphereTypeId() {
        return this.atmosphereTypeId;
    }

    public void setAtmosphereTypeId(Long atmosphereTypeId) {
        this.atmosphereTypeId = atmosphereTypeId;
    }

    public String getAtmosphereTypeName() {
        return this.atmosphereTypeName;
    }

    public void setAtmosphereTypeName(String atmosphereTypeName) {
        this.atmosphereTypeName = atmosphereTypeName == null ? null : atmosphereTypeName.intern();
    }

    public Long getTerraformingStateId() {
        return this.terraformingStateId;
    }

    public void setTerraformingStateId(Long terraformingStateId) {
        this.terraformingStateId = terraformingStateId;
    }

    public String getTerraformingStateName() {
        return this.terraformingStateName;
    }

    public void setTerraformingStateName(String terraformingStateName) {
        this.terraformingStateName = terraformingStateName == null ? null : terraformingStateName.intern();
    }

    public BigDecimal getEarthMasses() {
        return this.earthMasses;
    }

    public void setEarthMasses(BigDecimal earthMasses) {
        this.earthMasses = earthMasses;
    }

    public BigDecimal getRadius() {
        return this.radius;
    }

    public void setRadius(BigDecimal radius) {
        this.radius = radius;
    }

    public BigDecimal getGravity() {
        return this.gravity;
    }

    public void setGravity(BigDecimal gravity) {
        this.gravity = gravity;
    }

    public BigDecimal getSurfacePressure() {
        return this.surfacePressure;
    }

    public void setSurfacePressure(BigDecimal surfacePressure) {
        this.surfacePressure = surfacePressure;
    }

    public BigDecimal getOrbitalPeriod() {
        return this.orbitalPeriod;
    }

    public void setOrbitalPeriod(BigDecimal orbitalPeriod) {
        this.orbitalPeriod = orbitalPeriod;
    }

    public BigDecimal getSemiMajorAxis() {
        return this.semiMajorAxis;
    }

    public void setSemiMajorAxis(BigDecimal semiMajorAxis) {
        this.semiMajorAxis = semiMajorAxis;
    }

    public BigDecimal getOrbitalEccentricity() {
        return this.orbitalEccentricity;
    }

    public void setOrbitalEccentricity(BigDecimal orbitalEccentricity) {
        this.orbitalEccentricity = orbitalEccentricity;
    }

    public BigDecimal getOrbitalInclination() {
        return this.orbitalInclination;
    }

    public void setOrbitalInclination(BigDecimal orbitalInclination) {
        this.orbitalInclination = orbitalInclination;
    }

    public BigDecimal getArgOfPeriapsis() {
        return this.argOfPeriapsis;
    }

    public void setArgOfPeriapsis(BigDecimal argOfPeriapsis) {
        this.argOfPeriapsis = argOfPeriapsis;
    }

    public BigDecimal getRotationalPeriod() {
        return this.rotationalPeriod;
    }

    public void setRotationalPeriod(BigDecimal rotationalPeriod) {
        this.rotationalPeriod = rotationalPeriod;
    }

    public Boolean getIsRotationalPeriodTidallyLocked() {
        return this.isRotationalPeriodTidallyLocked;
    }

    public void setIsRotationalPeriodTidallyLocked(Boolean isRotationalPeriodTidallyLocked) {
        this.isRotationalPeriodTidallyLocked = isRotationalPeriodTidallyLocked;
    }

    public BigDecimal getAxisTilt() {
        return this.axisTilt;
    }

    public void setAxisTilt(BigDecimal axisTilt) {
        this.axisTilt = axisTilt;
    }

    public Long getEgId() {
        return this.egId;
    }

    public void setEgId(Long egId) {
        this.egId = egId;
    }

    public BigDecimal getBeltMoonMasses() {
        return this.beltMoonMasses;
    }

    public void setBeltMoonMasses(BigDecimal beltMoonMasses) {
        this.beltMoonMasses = beltMoonMasses;
    }

    public List<EddbRing> getRings() {
        return this.rings;
    }

    public void setRings(List<EddbRing> rings) {
        if (rings == null || rings.isEmpty()) {
            this.rings = null;
        } else {
            this.rings = new ArrayList<>(rings);
        }
    }

    public List<AtmosphereShare> getAtmosphereComposition() {
        return this.atmosphereComposition;
    }

    public void setAtmosphereComposition(List<AtmosphereShare> atmosphereComposition) {
        if (atmosphereComposition == null || atmosphereComposition.isEmpty()) {
            this.atmosphereComposition = null;
        } else {
            this.atmosphereComposition = new ArrayList<>(atmosphereComposition);
        }
    }

    public List<CompositionShare> getSolidComposition() {
        return this.solidComposition;
    }

    public void setSolidComposition(List<CompositionShare> solidComposition) {
        if (solidComposition == null || solidComposition.isEmpty()) {
            this.solidComposition = null;
        } else {
            this.solidComposition = new ArrayList<>(solidComposition);
        }
    }

    public List<MaterialShare> getMaterials() {
        return this.materials;
    }

    public void setMaterials(List<MaterialShare> materials) {
        if (materials == null || materials.isEmpty()) {
            this.materials = null;
        } else {
            this.materials = new ArrayList<>(materials);
        }
    }

    public Boolean getIsLandable() {
        return this.isLandable;
    }

    public void setIsLandable(Boolean isLandable) {
        this.isLandable = isLandable;
    }

    @Document(indexName = "eddb", type = "ring", shards = 10, replicas = 0)
    public static class EddbRing implements EddbEntity {

        private static final long serialVersionUID = 318036112349197244L;

        @Id
        @SerializedName("id")
        private Long id = null;
        @Field(type = FieldType.Date)
        @SerializedName("created_at")
        private Date createdAt = null;
        @Field(type = FieldType.Date)
        @SerializedName("updated_at")
        private Date updatedAt = null;
        @SerializedName("name")
        private String name = null;
        @SerializedName("ring_type_id")
        private Long ringTypeId = null;
        @SerializedName("ring_type_name")
        private String ringTypeName = null;
        @Field(type = FieldType.Double)
        @SerializedName("semi_major_axis")
        private BigDecimal semiMajorAxis = null;
        @Field(type = FieldType.Double)
        @SerializedName("ring_mass")
        private BigDecimal ringMass = null;
        @Field(type = FieldType.Double)
        @SerializedName("ring_inner_radius")
        private BigDecimal ringInnerRadius = null;
        @Field(type = FieldType.Double)
        @SerializedName("ring_outer_radius")
        private BigDecimal ringOuterRadius = null;

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (this.getClass() != obj.getClass()) {
                return false;
            }
            EddbRing other = (EddbRing) obj;
            if (this.id == null) {
                if (other.id != null) {
                    return false;
                }
            } else if (!this.id.equals(other.id)) {
                return false;
            }
            return true;
        }

        @Override
        public int hashCode() {
            return this.id.hashCode();
        }

        @Override
        public String toString() {
            return "#" + this.id + " " + this.name;
        }

        @Override
        public Long getId() {
            return this.id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public Date getCreatedAt() {
            return this.createdAt;
        }

        public void setCreatedAt(Date createdAt) {
            this.createdAt = createdAt;
        }

        public Date getUpdatedAt() {
            return this.updatedAt;
        }

        public void setUpdatedAt(Date updatedAt) {
            this.updatedAt = updatedAt;
        }

        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public Long getRingTypeId() {
            return this.ringTypeId;
        }

        public void setRingTypeId(Long ringTypeId) {
            this.ringTypeId = ringTypeId;
        }

        public String getRingTypeName() {
            return this.ringTypeName;
        }

        public void setRingTypeName(String ringTypeName) {
            this.ringTypeName = ringTypeName == null ? null : ringTypeName.intern();
        }

        public BigDecimal getSemiMajorAxis() {
            return this.semiMajorAxis;
        }

        public void setSemiMajorAxis(BigDecimal semiMajorAxis) {
            this.semiMajorAxis = semiMajorAxis;
        }

        public BigDecimal getRingMass() {
            return this.ringMass;
        }

        public void setRingMass(BigDecimal ringMass) {
            this.ringMass = ringMass;
        }

        public BigDecimal getRingInnerRadius() {
            return this.ringInnerRadius;
        }

        public void setRingInnerRadius(BigDecimal ringInnerRadius) {
            this.ringInnerRadius = ringInnerRadius;
        }

        public BigDecimal getRingOuterRadius() {
            return this.ringOuterRadius;
        }

        public void setRingOuterRadius(BigDecimal ringOuterRadius) {
            this.ringOuterRadius = ringOuterRadius;
        }

    }

    public static interface Share extends Serializable {

        Long getId();

        String getName();

        Float getShare();

    }

    public static class MaterialShare implements Share {

        private static final long serialVersionUID = -2202824775839173383L;

        @SerializedName("material_id")
        private Long id = null;
        @SerializedName("material_name")
        private String name = null;
        @SerializedName("share")
        private Float share = null;

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (this.getClass() != obj.getClass()) {
                return false;
            }
            MaterialShare other = (MaterialShare) obj;
            if (this.id == null) {
                if (other.id != null) {
                    return false;
                }
            } else if (!this.id.equals(other.id)) {
                return false;
            }
            return true;
        }

        @Override
        public int hashCode() {
            return this.id.hashCode();
        }

        @Override
        public String toString() {
            return "#" + this.id + " " + this.name;
        }

        @Override
        public Long getId() {
            return this.id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        @Override
        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name == null ? null : name.intern();
        }

        @Override
        public Float getShare() {
            return this.share;
        }

        public void setShare(Float share) {
            this.share = share;
        }

    }

    public static class AtmosphereShare implements Share {

        private static final long serialVersionUID = -2889005343256791057L;

        @SerializedName("atmosphere_component_id")
        private Long id = null;
        @SerializedName("atmosphere_component_name")
        private String name = null;
        @SerializedName("share")
        private Float share = null;

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (this.getClass() != obj.getClass()) {
                return false;
            }
            AtmosphereShare other = (AtmosphereShare) obj;
            if (this.id == null) {
                if (other.id != null) {
                    return false;
                }
            } else if (!this.id.equals(other.id)) {
                return false;
            }
            return true;
        }

        @Override
        public int hashCode() {
            return this.id.hashCode();
        }

        @Override
        public String toString() {
            return "#" + this.id + " " + this.name;
        }

        @Override
        public Long getId() {
            return this.id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        @Override
        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name == null ? null : name.intern();
        }

        @Override
        public Float getShare() {
            return this.share;
        }

        public void setShare(Float share) {
            this.share = share;
        }

    }

    public static class CompositionShare implements Share {

        private static final long serialVersionUID = 8614018149885747867L;

        @SerializedName("solid_component_id")
        private Long id = null;
        @SerializedName("solid_component_name")
        private String name = null;
        @SerializedName("share")
        private Float share = null;

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (this.getClass() != obj.getClass()) {
                return false;
            }
            CompositionShare other = (CompositionShare) obj;
            if (this.id == null) {
                if (other.id != null) {
                    return false;
                }
            } else if (!this.id.equals(other.id)) {
                return false;
            }
            return true;
        }

        @Override
        public int hashCode() {
            return this.id.hashCode();
        }

        @Override
        public String toString() {
            return "#" + this.id + " " + this.name;
        }

        @Override
        public Long getId() {
            return this.id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        @Override
        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name == null ? null : name.intern();
        }

        @Override
        public Float getShare() {
            return this.share;
        }

        public void setShare(Float share) {
            this.share = share;
        }

    }

}
