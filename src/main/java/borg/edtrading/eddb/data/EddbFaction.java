package borg.edtrading.eddb.data;

import com.google.gson.annotations.SerializedName;

import java.util.Date;

/**
 * EddbFaction
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbFaction implements EddbEntity {

    private static final long serialVersionUID = -243465206806544150L;

    private transient EddbSystem homeSystem = null;

    public EddbSystem getHomeSystem() {
        return this.homeSystem;
    }

    public void setHomeSystem(EddbSystem homeSystem) {
        this.homeSystem = homeSystem;
    }

    @SerializedName("id")
    private Long id = null;
    @SerializedName("updated_at")
    private Date updatedAt = null;
    @SerializedName("name")
    private String name = null;
    @SerializedName("government_id")
    private Long governmentId = null;
    @SerializedName("government")
    private String government = null;
    @SerializedName("allegiance_id")
    private Long allegianceId = null;
    @SerializedName("allegiance")
    private String allegiance = null;
    @SerializedName("state_id")
    private Long stateId = null;
    @SerializedName("state")
    private String state = null;
    @SerializedName("home_system_id")
    private Long homeSystemId = null;
    @SerializedName("is_player_faction")
    private Boolean isPlayerFaction = null;

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (this.getClass() != obj.getClass()) {
            return false;
        }
        EddbFaction other = (EddbFaction) obj;
        if (this.id == null) {
            if (other.id != null) {
                return false;
            }
        } else if (!this.id.equals(other.id)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return this.id.hashCode();
    }

    @Override
    public String toString() {
        return "#" + this.id + " " + this.name;
    }

    @Override
    public Long getId() {
        return this.id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Date getUpdatedAt() {
        return this.updatedAt;
    }

    public void setUpdatedAt(Date updatedAt) {
        this.updatedAt = updatedAt;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Long getGovernmentId() {
        return this.governmentId;
    }

    public void setGovernmentId(Long governmentId) {
        this.governmentId = governmentId;
    }

    public String getGovernment() {
        return this.government;
    }

    public void setGovernment(String government) {
        this.government = government == null ? null : government.intern();
    }

    public Long getAllegianceId() {
        return this.allegianceId;
    }

    public void setAllegianceId(Long allegianceId) {
        this.allegianceId = allegianceId;
    }

    public String getAllegiance() {
        return this.allegiance;
    }

    public void setAllegiance(String allegiance) {
        this.allegiance = allegiance == null ? null : allegiance.intern();
    }

    public Long getStateId() {
        return this.stateId;
    }

    public void setStateId(Long stateId) {
        this.stateId = stateId;
    }

    public String getState() {
        return this.state;
    }

    public void setState(String state) {
        this.state = state == null ? null : state.intern();
    }

    public Long getHomeSystemId() {
        return this.homeSystemId;
    }

    public void setHomeSystemId(Long homeSystemId) {
        this.homeSystemId = homeSystemId;
    }

    public Boolean getIsPlayerFaction() {
        return this.isPlayerFaction;
    }

    public void setIsPlayerFaction(Boolean isPlayerFaction) {
        this.isPlayerFaction = isPlayerFaction;
    }

}