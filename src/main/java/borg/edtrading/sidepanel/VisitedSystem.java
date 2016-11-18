package borg.edtrading.sidepanel;

import borg.edtrading.data.Coord;
import borg.edtrading.journal.entries.location.FSDJumpEntry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * VisitedSystem
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class VisitedSystem implements Serializable {

    private static final long serialVersionUID = 5638223986802210738L;

    static final Logger logger = LogManager.getLogger(VisitedSystem.class);

    private Date timestamp = null;
    private Coord coord = null;
    private String systemName = null;
    private boolean uninhabited = false;
    private int remainingPayout = 0;
    private List<ScannedBody> scannedBodies = new ArrayList<>(0);

    public VisitedSystem() {
        // Default
    }

    public VisitedSystem(FSDJumpEntry e) {
        this.setTimestamp(e.getTimestamp());
        this.setCoord(e.getStarPos());
        this.setSystemName(e.getStarSystem());
        this.setUninhabited("$government_None;".equals(e.getSystemGovernment()));
    }

    public ScannedBody lookupScannedBody(String bodyName) {
        for (ScannedBody scannedBody : this.getScannedBodies()) {
            if (scannedBody.getBodyName().equals(bodyName)) {
                return scannedBody;
            }
        }
        return null;
    }

    public Date getTimestamp() {
        return this.timestamp;
    }

    public void setTimestamp(Date timestamp) {
        this.timestamp = timestamp;
    }

    public Coord getCoord() {
        return this.coord;
    }

    public void setCoord(Coord coord) {
        this.coord = coord;
    }

    public String getSystemName() {
        return this.systemName;
    }

    public void setSystemName(String systemName) {
        this.systemName = systemName;
    }

    public boolean isUninhabited() {
        return this.uninhabited;
    }

    public void setUninhabited(boolean uninhabited) {
        this.uninhabited = uninhabited;
    }

    public int getRemainingPayout() {
        return this.remainingPayout;
    }

    public void setRemainingPayout(int remainingPayout) {
        this.remainingPayout = remainingPayout;
    }

    public List<ScannedBody> getScannedBodies() {
        return this.scannedBodies;
    }

    public void setScannedBodies(List<ScannedBody> scannedBodies) {
        this.scannedBodies = scannedBodies;
    }

}