package borg.edtrading.eddb.data;

import borg.edtrading.data.Coord;
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
 * EddbSystem
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
@Document(indexName = "eddb", type = "system")
public class EddbSystem implements EddbEntity {

    private static final long serialVersionUID = -2929675737765201360L;

    private transient Coord coord = null;
    private transient Float distanceFromSol = null;

    public Coord getCoord() {
        return this.coord;
    }

    public void setCoord(Coord coord) {
        this.coord = coord;
    }

    public Float getDistanceFromSol() {
        return this.distanceFromSol;
    }

    public void setDistanceFromSol(Float distanceFromSol) {
        this.distanceFromSol = distanceFromSol;
    }

    @Id
    @SerializedName("id")
    private Long id = null;
    @SerializedName("updated_at")
    private Date updatedAt = null;
    @SerializedName("edsm_id")
    private Long edsmId = null;
    @SerializedName("name")
    private String name = null;
    @SerializedName("simbad_ref")
    private String simbadRef = null;
    @SerializedName("x")
    private Float x = null;
    @SerializedName("y")
    private Float y = null;
    @SerializedName("z")
    private Float z = null;
    @Field(type = FieldType.Long)
    @SerializedName("population")
    private BigDecimal population = null;
    @SerializedName("is_populated")
    private Boolean isPopulated = null;
    @SerializedName("government_id")
    private Long governmentId = null;
    @SerializedName("government")
    private String government = null;
    @SerializedName("allegiance_id")
    private Long allegianceId = null;
    @SerializedName("allegiance")
    private String allegiance = null;
    @SerializedName("state_id")
    private Long stateId = null;
    @SerializedName("state")
    private String state = null;
    @SerializedName("security_id")
    private Long securityId = null;
    @SerializedName("security")
    private String security = null;
    @SerializedName("primary_economy_id")
    private Long primaryEconomyId = null;
    @SerializedName("primary_economy")
    private String primaryEconomy = null;
    @SerializedName("reserve_type_id")
    private Long reserveTypeId = null;
    @SerializedName("reserve_type")
    private String reserveType = null;
    @SerializedName("power")
    private String power = null;
    @SerializedName("power_state")
    private String powerState = null;
    @SerializedName("needs_permit")
    private Boolean needsPermit = null;
    @SerializedName("controlling_minor_faction_id")
    private Long controllingMinorFactionId = null;
    @SerializedName("controlling_minor_faction")
    private String controllingMinorFaction = null;
    @SerializedName("minor_faction_presences")
    private List<FactionPresence> minorFactionPresences = null;

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
        EddbSystem other = (EddbSystem) obj;
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

    public Date getUpdatedAt() {
        return this.updatedAt;
    }

    public void setUpdatedAt(Date updatedAt) {
        this.updatedAt = updatedAt;
    }

    public Long getEdsmId() {
        return this.edsmId;
    }

    public void setEdsmId(Long edsmId) {
        this.edsmId = edsmId;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSimbadRef() {
        return this.simbadRef;
    }

    public void setSimbadRef(String simbadRef) {
        this.simbadRef = simbadRef;
    }

    public Float getX() {
        return this.x;
    }

    public void setX(Float x) {
        this.x = x;
    }

    public Float getY() {
        return this.y;
    }

    public void setY(Float y) {
        this.y = y;
    }

    public Float getZ() {
        return this.z;
    }

    public void setZ(Float z) {
        this.z = z;
    }

    public BigDecimal getPopulation() {
        return this.population;
    }

    public void setPopulation(BigDecimal population) {
        this.population = population;
    }

    public Boolean getIsPopulated() {
        return this.isPopulated;
    }

    public void setIsPopulated(Boolean isPopulated) {
        this.isPopulated = isPopulated;
    }

    public Long getGovernmentId() {
        return this.governmentId;
    }

    public void setGovernmentId(Long governmentId) {
        this.governmentId = governmentId;
    }

    public String getGovernment() {
        return this.government;
    }

    public void setGovernment(String government) {
        this.government = government == null ? null : government.intern();
    }

    public Long getAllegianceId() {
        return this.allegianceId;
    }

    public void setAllegianceId(Long allegianceId) {
        this.allegianceId = allegianceId;
    }

    public String getAllegiance() {
        return this.allegiance;
    }

    public void setAllegiance(String allegiance) {
        this.allegiance = allegiance == null ? null : allegiance.intern();
    }

    public Long getStateId() {
        return this.stateId;
    }

    public void setStateId(Long stateId) {
        this.stateId = stateId;
    }

    public String getState() {
        return this.state;
    }

    public void setState(String state) {
        this.state = state == null ? null : state.intern();
    }

    public Long getSecurityId() {
        return this.securityId;
    }

    public void setSecurityId(Long securityId) {
        this.securityId = securityId;
    }

    public String getSecurity() {
        return this.security;
    }

    public void setSecurity(String security) {
        this.security = security == null ? null : security.intern();
    }

    public Long getPrimaryEconomyId() {
        return this.primaryEconomyId;
    }

    public void setPrimaryEconomyId(Long primaryEconomyId) {
        this.primaryEconomyId = primaryEconomyId;
    }

    public String getPrimaryEconomy() {
        return this.primaryEconomy;
    }

    public void setPrimaryEconomy(String primaryEconomy) {
        this.primaryEconomy = primaryEconomy == null ? null : primaryEconomy.intern();
    }

    public Long getReserveTypeId() {
        return this.reserveTypeId;
    }

    public void setReserveTypeId(Long reserveTypeId) {
        this.reserveTypeId = reserveTypeId;
    }

    public String getReserveType() {
        return this.reserveType;
    }

    public void setReserveType(String reserveType) {
        this.reserveType = reserveType == null ? null : reserveType.intern();
    }

    public String getPower() {
        return this.power;
    }

    public void setPower(String power) {
        this.power = power == null ? null : power.intern();
    }

    public String getPowerState() {
        return this.powerState;
    }

    public void setPowerState(String powerState) {
        this.powerState = powerState == null ? null : powerState.intern();
    }

    public Boolean getNeedsPermit() {
        return this.needsPermit;
    }

    public void setNeedsPermit(Boolean needsPermit) {
        this.needsPermit = needsPermit;
    }

    public Long getControllingMinorFactionId() {
        return this.controllingMinorFactionId;
    }

    public void setControllingMinorFactionId(Long controllingMinorFactionId) {
        this.controllingMinorFactionId = controllingMinorFactionId;
    }

    public String getControllingMinorFaction() {
        return this.controllingMinorFaction;
    }

    public void setControllingMinorFaction(String controllingMinorFaction) {
        this.controllingMinorFaction = controllingMinorFaction;
    }

    public List<FactionPresence> getMinorFactionPresences() {
        return this.minorFactionPresences;
    }

    public void setMinorFactionPresences(List<FactionPresence> minorFactionPresences) {
        if (minorFactionPresences == null || minorFactionPresences.isEmpty()) {
            this.minorFactionPresences = null;
        } else {
            this.minorFactionPresences = new ArrayList<>(minorFactionPresences);
        }
    }

    public static class FactionPresence implements Serializable {

        private static final long serialVersionUID = -3357612311369294560L;

        @SerializedName("minor_faction_id")
        private Long id = null;
        @SerializedName("state_id")
        private Long stateId = null;
        @SerializedName("state")
        private String state = null;
        @SerializedName("influence")
        private Float influence = null;

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
            FactionPresence other = (FactionPresence) obj;
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
            return "#" + this.id;
        }

        public Long getId() {
            return this.id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public Long getStateId() {
            return this.stateId;
        }

        public void setStateId(Long stateId) {
            this.stateId = stateId;
        }

        public String getState() {
            return this.state;
        }

        public void setState(String state) {
            this.state = state == null ? null : state.intern();
        }

        public Float getInfluence() {
            return this.influence;
        }

        public void setInfluence(Float influence) {
            this.influence = influence;
        }

    }

}
