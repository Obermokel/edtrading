package borg.edtrading.eddb.data;

import com.google.gson.annotations.SerializedName;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.List;

/**
 * Station
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Station implements Serializable {

    private static final long serialVersionUID = -6292312329063206773L;

    static final Logger logger = LogManager.getLogger(Station.class);

    private Long id = null;
    private String name = null;
    @SerializedName("max_landing_pad_size")
    private String maxLandingPadSize = null;
    @SerializedName("system_id")
    private Long starSystemId = null;
    private StarSystem starSystem = null;
    @SerializedName("distance_to_star")
    private float distanceFromStarInLs = 0.0f;
    private String allegiance = null;
    private String state = null;
    private List<String> economies = null;
    @SerializedName("is_planetary")
    private boolean planetary = false;
    @SerializedName("has_blackmarket")
    private Boolean hasBlackmarket = null;
    @SerializedName("has_market")
    private Boolean hasMarket = null;
    @SerializedName("has_refuel")
    private Boolean hasRefuel = null;
    @SerializedName("has_repair")
    private Boolean hasRepair = null;
    @SerializedName("has_rearm")
    private Boolean hasRearm = null;
    @SerializedName("has_outfitting")
    private Boolean hasOutfitting = null;
    @SerializedName("has_shipyard")
    private Boolean hasShipyard = null;
    @SerializedName("has_docking")
    private Boolean hasDocking = null;
    @SerializedName("has_commodities")
    private Boolean hasCommodities = null;
    @SerializedName("controlling_minor_faction_id")
    private Long controllingMinorFactionId = null;
    private Faction controllingMinorFaction = null;

    public Station() {
        // Default
    }

    public Station(String name, StarSystem starSystem, float distanceFromStarInLs) {
        this.setName(name);
        this.setStarSystem(starSystem);
        this.setDistanceFromStarInLs(distanceFromStarInLs);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        Station other = (Station) obj;
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
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
        return result;
    }

    @Override
    public String toString() {
        return String.format("#%d %s", this.getId(), this.getName());
    }

    public Long getId() {
        return this.id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Long getStarSystemId() {
        return this.starSystemId;
    }

    public void setStarSystemId(Long starSystemId) {
        this.starSystemId = starSystemId;
    }

    public StarSystem getStarSystem() {
        return this.starSystem;
    }

    public void setStarSystem(StarSystem starSystem) {
        this.starSystem = starSystem;
    }

    public float getDistanceFromStarInLs() {
        return this.distanceFromStarInLs;
    }

    public void setDistanceFromStarInLs(float distanceFromStarInLs) {
        this.distanceFromStarInLs = distanceFromStarInLs;
    }

    public String getMaxLandingPadSize() {
        return this.maxLandingPadSize;
    }

    public void setMaxLandingPadSize(String maxLandingPadSize) {
        this.maxLandingPadSize = maxLandingPadSize;
    }

    public String getAllegiance() {
        return this.allegiance;
    }

    public void setAllegiance(String allegiance) {
        this.allegiance = allegiance;
    }

    public String getState() {
        return this.state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public List<String> getEconomies() {
        return this.economies;
    }

    public void setEconomies(List<String> economies) {
        this.economies = economies;
    }

    public boolean isPlanetary() {
        return this.planetary;
    }

    public void setPlanetary(boolean planetary) {
        this.planetary = planetary;
    }

    public Boolean getHasBlackmarket() {
        return this.hasBlackmarket;
    }

    public void setHasBlackmarket(Boolean hasBlackmarket) {
        this.hasBlackmarket = hasBlackmarket;
    }

    public Boolean getHasMarket() {
        return this.hasMarket;
    }

    public void setHasMarket(Boolean hasMarket) {
        this.hasMarket = hasMarket;
    }

    public Boolean getHasRefuel() {
        return this.hasRefuel;
    }

    public void setHasRefuel(Boolean hasRefuel) {
        this.hasRefuel = hasRefuel;
    }

    public Boolean getHasRepair() {
        return this.hasRepair;
    }

    public void setHasRepair(Boolean hasRepair) {
        this.hasRepair = hasRepair;
    }

    public Boolean getHasRearm() {
        return this.hasRearm;
    }

    public void setHasRearm(Boolean hasRearm) {
        this.hasRearm = hasRearm;
    }

    public Boolean getHasOutfitting() {
        return this.hasOutfitting;
    }

    public void setHasOutfitting(Boolean hasOutfitting) {
        this.hasOutfitting = hasOutfitting;
    }

    public Boolean getHasShipyard() {
        return this.hasShipyard;
    }

    public void setHasShipyard(Boolean hasShipyard) {
        this.hasShipyard = hasShipyard;
    }

    public Boolean getHasDocking() {
        return this.hasDocking;
    }

    public void setHasDocking(Boolean hasDocking) {
        this.hasDocking = hasDocking;
    }

    public Boolean getHasCommodities() {
        return this.hasCommodities;
    }

    public void setHasCommodities(Boolean hasCommodities) {
        this.hasCommodities = hasCommodities;
    }

    public Long getControllingMinorFactionId() {
        return this.controllingMinorFactionId;
    }

    public void setControllingMinorFactionId(Long controllingMinorFactionId) {
        this.controllingMinorFactionId = controllingMinorFactionId;
    }

    public Faction getControllingMinorFaction() {
        return this.controllingMinorFaction;
    }

    public void setControllingMinorFaction(Faction controllingMinorFaction) {
        this.controllingMinorFaction = controllingMinorFaction;
    }

}
