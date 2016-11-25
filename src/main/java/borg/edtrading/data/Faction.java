package borg.edtrading.data;

import com.google.gson.annotations.SerializedName;

import java.io.Serializable;
import java.util.Date;

/**
 * Faction
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Faction implements Serializable {

    private static final long serialVersionUID = 8867683874649271927L;

    private Long id = null;
    private String name = null;
    @SerializedName("updated_at")
    private Date updatedAt = null;
    private String government = null;
    private String allegiance = null;
    private String state = null;
    @SerializedName("home_system_id")
    private Long homeSystemId = null;
    private StarSystem homeSystem = null;
    @SerializedName("is_player_faction")
    private boolean playerFaction = false;

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        Faction other = (Faction) obj;
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
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
        return result;
    }

    @Override
    public String toString() {
        return String.format("#%d %s", this.getId(), this.getName());
    }

    public Long getId() {
        return this.id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Date getUpdatedAt() {
        return this.updatedAt;
    }

    public void setUpdatedAt(Date updatedAt) {
        this.updatedAt = updatedAt;
    }

    public String getGovernment() {
        return this.government;
    }

    public void setGovernment(String government) {
        this.government = government;
    }

    public String getAllegiance() {
        return this.allegiance;
    }

    public void setAllegiance(String allegiance) {
        this.allegiance = allegiance;
    }

    public String getState() {
        return this.state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public Long getHomeSystemId() {
        return this.homeSystemId;
    }

    public void setHomeSystemId(Long homeSystemId) {
        this.homeSystemId = homeSystemId;
    }

    public StarSystem getHomeSystem() {
        return this.homeSystem;
    }

    public void setHomeSystem(StarSystem homeSystem) {
        this.homeSystem = homeSystem;
    }

    public boolean isPlayerFaction() {
        return this.playerFaction;
    }

    public void setPlayerFaction(boolean playerFaction) {
        this.playerFaction = playerFaction;
    }

}
