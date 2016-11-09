package borg.edtrading.journal;

import borg.edtrading.data.Coord;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * LocationEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class LocationEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -8552947523934722210L;

    private final Boolean docked;
    private final String stationName;
    private final String stationType;
    private final String starSystem;
    private final Coord starPos;
    private final String allegiance;
    private final String economy;
    private final String economyLocalized;
    private final String government;
    private final String governmentLocalized;
    private final String security;
    private final String securityLocalized;
    private final String body;
    private final String bodyType;
    private final String faction;
    private final String factionState;

    public LocationEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.docked = this.readBoolean(data, "Docked");
        this.stationName = this.readString(data, "StationName");
        this.stationType = this.readString(data, "StationType");
        this.starSystem = this.readString(data, "StarSystem");
        this.starPos = this.readCoord(data, "StarPos");
        this.allegiance = this.readString(data, "Allegiance");
        this.economy = this.readString(data, "Economy");
        this.economyLocalized = this.readString(data, "Economy_Localised");
        this.government = this.readString(data, "Government");
        this.governmentLocalized = this.readString(data, "Government_Localised");
        this.security = this.readString(data, "Security");
        this.securityLocalized = this.readString(data, "Security_Localised");
        this.body = this.readString(data, "Body");
        this.bodyType = this.readString(data, "BodyType");
        this.faction = this.readString(data, "Faction");
        this.factionState = this.readString(data, "FactionState");
    }

    public Boolean getDocked() {
        return this.docked;
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

    public String getBody() {
        return this.body;
    }

    public String getBodyType() {
        return this.bodyType;
    }

    public String getFaction() {
        return this.faction;
    }

    public String getFactionState() {
        return this.factionState;
    }

}
