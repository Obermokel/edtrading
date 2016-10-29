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
    private double totalDistanceLy = 0;

    public Path(StarSystem starSystem) {
        this.setStarSystem(starSystem);
    }

    /**
     * @param extraDistanceLy
     *            From prev to starSystem, NOT in total
     */
    public Path(Path prev, StarSystem starSystem, double extraDistanceLy) {
        this.setPrev(prev);
        this.setStarSystem(starSystem);
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
        if (Double.doubleToLongBits(this.totalDistanceLy) != Double.doubleToLongBits(other.totalDistanceLy)) {
            return false;
        }
        if (this.prev == null) {
            if (other.prev != null) {
                return false;
            }
        } else if (!this.prev.equals(other.prev)) {
            return false;
        }
        if (this.starSystem == null) {
            if (other.starSystem != null) {
                return false;
            }
        } else if (!this.starSystem.equals(other.starSystem)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        long temp;
        temp = Double.doubleToLongBits(this.totalDistanceLy);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result + ((this.prev == null) ? 0 : this.prev.hashCode());
        result = prime * result + ((this.starSystem == null) ? 0 : this.starSystem.hashCode());
        return result;
    }

    @Override
    public int compareTo(Path other) {
        return new Integer(this.getTotalHops()).compareTo(other.getTotalHops());
    }

    public int getTotalHops() {
        int hops = 0;
        Path p = this.prev;
        while (p != null) {
            hops++;
            p = p.prev;
        }
        return hops;
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

    public double getTotalDistanceLy() {
        return this.totalDistanceLy;
    }

    public void setTotalDistanceLy(double ly) {
        this.totalDistanceLy = ly;
    }

}
