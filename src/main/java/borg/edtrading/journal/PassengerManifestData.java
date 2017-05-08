package borg.edtrading.journal;

import java.io.Serializable;

/**
 * PassengerManifestData
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PassengerManifestData implements Serializable {

    private static final long serialVersionUID = 3547317076378585849L;

    private final String type;
    private final Boolean vip;
    private final Boolean wanted;
    private final Integer count;
    private final Integer missionID;

    public PassengerManifestData(String type, Boolean vip, Boolean wanted, Integer count, Integer missionID) {
        this.type = type;
        this.vip = vip;
        this.wanted = wanted;
        this.count = count;
        this.missionID = missionID;
    }

    public String getType() {
        return this.type;
    }

    public Boolean getVip() {
        return this.vip;
    }

    public Boolean getWanted() {
        return this.wanted;
    }

    public Integer getCount() {
        return this.count;
    }

    public Integer getMissionID() {
        return this.missionID;
    }

}
