package borg.edtrading.journal;

import borg.edtrading.data.Coord;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * Jumping from one system to another
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FSDJumpEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 8448113468495755106L;

    private final String starSystem;
    private final Coord starPos;
    private final String allegiance;
    private final String economy;
    private final String economyLocalized;
    private final String government;
    private final String governmentLocalized;
    private final String security;
    private final String securityLocalized;
    private final String faction;
    private final String factionState;
    private final Float jumpDist;
    private final Float fuelUsed;
    private final Float fuelLevel;
    private final Float boostUsed;

    public FSDJumpEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.starSystem = this.readString(data, "StarSystem");
        this.starPos = this.readCoord(data, "StarPos");
        this.allegiance = this.readString(data, "Allegiance");
        this.economy = this.readString(data, "Economy");
        this.economyLocalized = this.readString(data, "Economy_Localised");
        this.government = this.readString(data, "Government");
        this.governmentLocalized = this.readString(data, "Government_Localised");
        this.security = this.readString(data, "Security");
        this.securityLocalized = this.readString(data, "Security_Localised");
        this.faction = this.readString(data, "Faction");
        this.factionState = this.readString(data, "FactionState");
        this.jumpDist = this.readFloat(data, "JumpDist");
        this.fuelUsed = this.readFloat(data, "FuelUsed");
        this.fuelLevel = this.readFloat(data, "FuelLevel");
        this.boostUsed = this.readFloat(data, "BoostUsed");
    }

    public String getStarSystem() {
        return this.starSystem;
    }

    public Coord getStarPos() {
        return this.starPos;
    }

    public String getAllegiance() {
        return this.allegiance;
    }

    public String getEconomy() {
        return this.economy;
    }

    public String getEconomyLocalized() {
        return this.economyLocalized;
    }

    public String getGovernment() {
        return this.government;
    }

    public String getGovernmentLocalized() {
        return this.governmentLocalized;
    }

    public String getSecurity() {
        return this.security;
    }

    public String getSecurityLocalized() {
        return this.securityLocalized;
    }

    public String getFaction() {
        return this.faction;
    }

    public String getFactionState() {
        return this.factionState;
    }

    /**
     * Ly
     */
    public Float getJumpDist() {
        return this.jumpDist;
    }

    /**
     * Tons
     */
    public Float getFuelUsed() {
        return this.fuelUsed;
    }

    /**
     * Tons (after the jump destination is reached)
     */
    public Float getFuelLevel() {
        return this.fuelLevel;
    }

    /**
     * 4.0 (= +300%) for neutron star boost
     */
    public Float getBoostUsed() {
        return this.boostUsed;
    }

}
