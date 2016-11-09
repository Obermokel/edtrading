package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * LoadGameEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class LoadGameEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -5092326492135999307L;

    private final String commander;
    private final String ship;
    private final Integer shipID;
    private final String gameMode;
    private final String group;
    private final Integer credits;
    private final Integer loan;
    private final Boolean startLanded;

    public LoadGameEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.commander = this.readString(data, "Commander");
        this.ship = this.readString(data, "Ship");
        this.shipID = this.readInt(data, "ShipID");
        this.gameMode = this.readString(data, "GameMode");
        this.group = this.readString(data, "Group");
        this.credits = this.readInt(data, "Credits");
        this.loan = this.readInt(data, "Loan");
        this.startLanded = this.readBoolean(data, "StartLanded");
    }

    public String getCommander() {
        return this.commander;
    }

    public String getShip() {
        return this.ship;
    }

    public Integer getShipID() {
        return this.shipID;
    }

    public String getGameMode() {
        return this.gameMode;
    }

    public String getGroup() {
        return this.group;
    }

    public Integer getCredits() {
        return this.credits;
    }

    public Integer getLoan() {
        return this.loan;
    }

    public Boolean getStartLanded() {
        return this.startLanded;
    }

}
