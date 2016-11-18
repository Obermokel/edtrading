package borg.edtrading.sidepanel;

import borg.edtrading.journal.entries.exploration.ScanEntry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.Date;

/**
 * ScannedBody
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScannedBody implements Serializable {

    private static final long serialVersionUID = 6942164845646742037L;

    static final Logger logger = LogManager.getLogger(ScannedBody.class);

    private Date timestamp = null;
    private String bodyName = null;
    private String bodyClass = null;
    private boolean discovered = false;
    private int remainingBasePayout = 0;
    private int remainingBonusPayout = 0;

    public ScannedBody() {
        // Default
    }

    public ScannedBody(ScanEntry e) {
        this.setTimestamp(e.getTimestamp());
        this.setBodyName(e.getBodyName());
        this.setBodyClass(ScanEntry.toBodyClass(e));
    }

    public Date getTimestamp() {
        return this.timestamp;
    }

    public void setTimestamp(Date timestamp) {
        this.timestamp = timestamp;
    }

    public String getBodyName() {
        return this.bodyName;
    }

    public void setBodyName(String bodyName) {
        this.bodyName = bodyName;
    }

    public String getBodyClass() {
        return this.bodyClass;
    }

    public void setBodyClass(String bodyClass) {
        this.bodyClass = bodyClass;
    }

    public boolean isDiscovered() {
        return this.discovered;
    }

    public void setDiscovered(boolean discovered) {
        this.discovered = discovered;

        if (discovered) {
            this.setRemainingBonusPayout(this.getRemainingBasePayout() / 4);
        } else {
            this.setRemainingBonusPayout(0);
        }
    }

    public int getRemainingBasePayout() {
        return this.remainingBasePayout;
    }

    public void setRemainingBasePayout(int remainingBasePayout) {
        this.remainingBasePayout = remainingBasePayout;
    }

    public int getRemainingBonusPayout() {
        return this.remainingBonusPayout;
    }

    public void setRemainingBonusPayout(int remainingBonusPayout) {
        this.remainingBonusPayout = remainingBonusPayout;
    }

}
