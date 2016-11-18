package borg.edtrading.journal.entries;

import borg.edtrading.data.Coord;
import borg.edtrading.journal.Event;

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
    private final String systemAllegiance;
    private final String systemEconomy;
    private final String systemEconomyLocalized;
    private final String systemGovernment;
    private final String systemGovernmentLocalized;
    private final String systemSecurity;
    private final String systemSecurityLocalized;
    private final String systemFaction;
    private final String factionState;
    private final Float jumpDist;
    private final Float fuelUsed;
    private final Float fuelLevel;
    private final Float boostUsed;

    public FSDJumpEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.starSystem = this.readString(data, "StarSystem");
        this.starPos = this.readCoord(data, "StarPos");
        this.systemAllegiance = data.containsKey("Allegiance") ? this.readString(data, "Allegiance") : this.readString(data, "SystemAllegiance");
        this.systemEconomy = data.containsKey("Economy") ? this.readString(data, "Economy") : this.readString(data, "SystemEconomy");
        this.systemEconomyLocalized = data.containsKey("Economy_Localised") ? this.readString(data, "Economy_Localised") : this.readString(data, "SystemEconomy_Localised");
        this.systemGovernment = data.containsKey("Government") ? this.readString(data, "Government") : this.readString(data, "SystemGovernment");
        this.systemGovernmentLocalized = data.containsKey("Government_Localised") ? this.readString(data, "Government_Localised") : this.readString(data, "SystemGovernment_Localised");
        this.systemSecurity = data.containsKey("Security") ? this.readString(data, "Security") : this.readString(data, "SystemSecurity");
        this.systemSecurityLocalized = data.containsKey("Security_Localised") ? this.readString(data, "Security_Localised") : this.readString(data, "SystemSecurity_Localised");
        this.systemFaction = data.containsKey("Faction") ? this.readString(data, "Faction") : this.readString(data, "SystemFaction");
        this.factionState = this.readString(data, "FactionState");
        this.jumpDist = this.readFloat(data, "JumpDist");
        this.fuelUsed = this.readFloat(data, "FuelUsed");
        this.fuelLevel = this.readFloat(data, "FuelLevel");
        this.boostUsed = this.readFloat(data, "BoostUsed");
    }

    /**
     * Destination system to which we are jumping
     */
    public String getStarSystem() {
        return this.starSystem;
    }

    public Coord getStarPos() {
        return this.starPos;
    }

    public String getSystemAllegiance() {
        return this.systemAllegiance;
    }

    public String getSystemEconomy() {
        return this.systemEconomy;
    }

    public String getSystemEconomyLocalized() {
        return this.systemEconomyLocalized;
    }

    public String getSystemGovernment() {
        return this.systemGovernment;
    }

    public String getSystemGovernmentLocalized() {
        return this.systemGovernmentLocalized;
    }

    public String getSystemSecurity() {
        return this.systemSecurity;
    }

    public String getSystemSecurityLocalized() {
        return this.systemSecurityLocalized;
    }

    public String getSystemFaction() {
        return this.systemFaction;
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
