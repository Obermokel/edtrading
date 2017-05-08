package borg.edtrading.journal.entries.location;

import borg.edtrading.data.Coord;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

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
    private final String body;
    private final String bodyType;
    private final String systemAllegiance;
    private final String systemEconomy;
    private final String systemEconomyLocalized;
    private final String systemGovernment;
    private final String systemGovernmentLocalized;
    private final String systemSecurity;
    private final String systemSecurityLocalized;
    private final String systemFaction;
    private final String factionState;
    private final List<String> powers;
    private final String powerplayState;
    private final List<Faction> factions;

    public LocationEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.docked = this.readBoolean(data, "Docked");
        this.stationName = this.readString(data, "StationName");
        this.stationType = this.readString(data, "StationType");
        this.starSystem = this.readString(data, "StarSystem");
        this.starPos = this.readCoord(data, "StarPos");
        this.body = this.readString(data, "Body");
        this.bodyType = this.readString(data, "BodyType");
        this.systemAllegiance = data.containsKey("Allegiance") ? this.readString(data, "Allegiance") : this.readString(data, "SystemAllegiance");
        this.systemEconomy = data.containsKey("Economy") ? this.readString(data, "Economy") : this.readString(data, "SystemEconomy");
        this.systemEconomyLocalized = data.containsKey("Economy_Localised") ? this.readString(data, "Economy_Localised") : this.readString(data, "SystemEconomy_Localised");
        this.systemGovernment = data.containsKey("Government") ? this.readString(data, "Government") : this.readString(data, "SystemGovernment");
        this.systemGovernmentLocalized = data.containsKey("Government_Localised") ? this.readString(data, "Government_Localised") : this.readString(data, "SystemGovernment_Localised");
        this.systemSecurity = data.containsKey("Security") ? this.readString(data, "Security") : this.readString(data, "SystemSecurity");
        this.systemSecurityLocalized = data.containsKey("Security_Localised") ? this.readString(data, "Security_Localised") : this.readString(data, "SystemSecurity_Localised");
        this.systemFaction = data.containsKey("Faction") ? this.readString(data, "Faction") : this.readString(data, "SystemFaction");
        this.factionState = this.readString(data, "FactionState");
        this.powers = this.readList(data, "Powers", String.class);
        this.powerplayState = this.readString(data, "PowerplayState");
        this.factions = this.readFactions(data, "Factions");
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

    public String getBody() {
        return this.body;
    }

    public String getBodyType() {
        return this.bodyType;
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

    public List<String> getPowers() {
        return this.powers;
    }

    public String getPowerplayState() {
        return this.powerplayState;
    }

    public List<Faction> getFactions() {
        return this.factions;
    }

}
