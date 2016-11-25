package borg.edtrading.aystar;

import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.StarSystem;

import java.io.Serializable;

/**
 * MinimizedStarSystem
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MinimizedStarSystem implements Serializable {

    private static final long serialVersionUID = -3492099076880874005L;

    private final Long id;
    private final Coord coord;

    public MinimizedStarSystem(StarSystem fullStarSystem) {
        this.id = fullStarSystem.getId();
        this.coord = fullStarSystem.getCoord();
    }

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
        MinimizedStarSystem other = (MinimizedStarSystem) obj;
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
        return String.format("#%d (%s)", this.getId(), this.getCoord());
    }

    public float distanceTo(MinimizedStarSystem other) {
        return this.getCoord().distanceTo(other.getCoord());
    }

    public Long getId() {
        return this.id;
    }

    public Coord getCoord() {
        return this.coord;
    }

}
