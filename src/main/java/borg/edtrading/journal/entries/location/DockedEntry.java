package borg.edtrading.journal.entries.location;

import borg.edtrading.data.Coord;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * DockedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DockedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -5020421222455569735L;

    private final String starSystem;
    private final Coord starPos;
    private final Float distFromStarLS;
    private final String stationName;
    private final String stationType;
    private final String stationFaction;
    private final String stationAllegiance;
    private final String stationEconomy;
    private final String stationEconomyLocalized;
    private final String stationGovernment;
    private final String stationGovernmentLocalized;
    private final String systemSecurity;
    private final String systemSecurityLocalized;
    private final String factionState;
    private final Boolean cockpitBreach;
    private final List<Faction> factions;

    public DockedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.starSystem = this.readString(data, "StarSystem");
        this.starPos = this.readCoord(data, "StarPos");
        this.distFromStarLS = this.readFloat(data, "DistFromStarLS");
        this.stationName = this.readString(data, "StationName");
        this.stationType = this.readString(data, "StationType");
        this.stationAllegiance = data.containsKey("Allegiance") ? this.readString(data, "Allegiance") : this.readString(data, "StationAllegiance");
        this.stationEconomy = data.containsKey("Economy") ? this.readString(data, "Economy") : this.readString(data, "StationEconomy");
        this.stationEconomyLocalized = data.containsKey("Economy_Localised") ? this.readString(data, "Economy_Localised") : this.readString(data, "StationEconomy_Localised");
        this.stationGovernment = data.containsKey("Government") ? this.readString(data, "Government") : this.readString(data, "StationGovernment");
        this.stationGovernmentLocalized = data.containsKey("Government_Localised") ? this.readString(data, "Government_Localised") : this.readString(data, "StationGovernment_Localised");
        this.stationFaction = data.containsKey("Faction") ? this.readString(data, "Faction") : this.readString(data, "StationFaction");
        this.systemSecurity = data.containsKey("Security") ? this.readString(data, "Security") : this.readString(data, "SystemSecurity");
        this.systemSecurityLocalized = data.containsKey("Security_Localised") ? this.readString(data, "Security_Localised") : this.readString(data, "SystemSecurity_Localised");
        this.factionState = this.readString(data, "FactionState");
        this.cockpitBreach = this.readBoolean(data, "CockpitBreach");
        this.factions = this.readFactions(data, "Factions");
    }

    public String getStarSystem() {
        return this.starSystem;
    }

    public Coord getStarPos() {
        return this.starPos;
    }

    public Float getDistFromStarLS() {
        return this.distFromStarLS;
    }

    public String getStationName() {
        return this.stationName;
    }

    public String getStationType() {
        return this.stationType;
    }

    public String getStationFaction() {
        return this.stationFaction;
    }

    public String getStationAllegiance() {
        return this.stationAllegiance;
    }

    public String getStationEconomy() {
        return this.stationEconomy;
    }

    public String getStationEconomyLocalized() {
        return this.stationEconomyLocalized;
    }

    public String getStationGovernment() {
        return this.stationGovernment;
    }

    public String getStationGovernmentLocalized() {
        return this.stationGovernmentLocalized;
    }

    public String getSystemSecurity() {
        return this.systemSecurity;
    }

    public String getSystemSecurityLocalized() {
        return this.systemSecurityLocalized;
    }

    public String getFactionState() {
        return this.factionState;
    }

    public Boolean getCockpitBreach() {
        return this.cockpitBreach;
    }

    public List<Faction> getFactions() {
        return this.factions;
    }

}
