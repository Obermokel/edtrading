package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * DockedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DockedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -5020421222455569735L;

    private final String stationName;
    private final String stationType;
    private final String starSystem;
    private final String faction;
    private final String factionState;
    private final String allegiance;
    private final String economy;
    private final String economyLocalized;
    private final String government;
    private final String governmentLocalized;
    private final String security;
    private final String securityLocalized;

    public DockedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.stationName = this.readString(data, "StationName");
        this.stationType = this.readString(data, "StationType");
        this.starSystem = this.readString(data, "StarSystem");
        this.faction = this.readString(data, "Faction");
        this.factionState = this.readString(data, "FactionState");
        this.allegiance = this.readString(data, "Allegiance");
        this.economy = this.readString(data, "Economy");
        this.economyLocalized = this.readString(data, "Economy_Localised");
        this.government = this.readString(data, "Government");
        this.governmentLocalized = this.readString(data, "Government_Localised");
        this.security = this.readString(data, "Security");
        this.securityLocalized = this.readString(data, "Security_Localised");
    }

    public String getStationName() {
        return this.stationName;
    }

    public String getStationType() {
        return this.stationType;
    }

    public String getStarSystem() {
        return this.starSystem;
    }

    public String getFaction() {
        return this.faction;
    }

    public String getFactionState() {
        return this.factionState;
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

}
