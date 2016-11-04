package borg.edtrading.aystar;

import borg.edtrading.data.StarSystem;

/**
 * Path
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Path implements Comparable<Path> {

    private Path prev = null;
    private StarSystem starSystem = null;
    private int totalJumps = 0;
    private float totalDistanceLy = 0;

    public Path(StarSystem starSystem) {
        this.setStarSystem(starSystem);
    }

    /**
     * @param extraDistanceLy
     *            From prev to starSystem, NOT in total
     */
    public Path(Path prev, StarSystem starSystem, float extraDistanceLy) {
        this.setPrev(prev);
        this.setStarSystem(starSystem);
        this.setTotalJumps(prev.getTotalJumps() + 1);
        this.setTotalDistanceLy(prev.getTotalDistanceLy() + extraDistanceLy);
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
        Path other = (Path) obj;
        if (Float.floatToIntBits(this.totalDistanceLy) != Float.floatToIntBits(other.totalDistanceLy)) {
            return false;
        }
        if (this.totalJumps != other.totalJumps) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Float.floatToIntBits(this.totalDistanceLy);
        result = prime * result + this.totalJumps;
        return result;
    }

    @Override
    public int compareTo(Path other) {
        int byDistance = new Float(this.getTotalDistanceLy()).compareTo(other.getTotalDistanceLy());
        if (byDistance != 0) {
            return byDistance;
        } else {
            int byJumps = new Integer(this.getTotalJumps()).compareTo(other.getTotalJumps());
            return byJumps;
        }
    }

    public Path getPrev() {
        return this.prev;
    }

    public void setPrev(Path prev) {
        this.prev = prev;
    }

    public StarSystem getStarSystem() {
        return this.starSystem;
    }

    public void setStarSystem(StarSystem starSystem) {
        this.starSystem = starSystem;
    }

    public int getTotalJumps() {
        return this.totalJumps;
    }

    public void setTotalJumps(int totalJumps) {
        this.totalJumps = totalJumps;
    }

    public float getTotalDistanceLy() {
        return this.totalDistanceLy;
    }

    public void setTotalDistanceLy(float ly) {
        this.totalDistanceLy = ly;
    }

}
