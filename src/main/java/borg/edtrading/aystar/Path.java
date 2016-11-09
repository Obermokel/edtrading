package borg.edtrading.aystar;

import borg.edtrading.data.StarSystem;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Path
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Path implements Comparable<Path> {

    private Path prev = null;
    private StarSystem starSystem = null;
    private int totalJumps = 0;
    private float travelledDistanceLy = 0;
    private float remainingDistanceLy = 0;
    private float fuelLevel = 0;

    public Path(StarSystem starSystem, float remainingDistanceLy, float fuelLevel) {
        this.setStarSystem(starSystem);
        this.setRemainingDistanceLy(remainingDistanceLy);
        this.setFuelLevel(fuelLevel);
    }

    /**
     * @param extraTravelledDistanceLy
     *            From prev to starSystem, NOT in total
     */
    public Path(Path prev, StarSystem starSystem, float remainingDistanceLy, float extraTravelledDistanceLy, float fuelLevel) {
        this.setPrev(prev);
        this.setStarSystem(starSystem);
        this.setRemainingDistanceLy(remainingDistanceLy);
        this.setTotalJumps(prev.getTotalJumps() + 1);
        this.setTravelledDistanceLy(prev.getTravelledDistanceLy() + extraTravelledDistanceLy);
        this.setFuelLevel(fuelLevel);
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
        if (this.starSystem.getId().longValue() != other.starSystem.getId().longValue()) {
            return false;
        }
        if (this.totalJumps != other.totalJumps) {
            return false;
        }
        if (Float.floatToIntBits(this.travelledDistanceLy) != Float.floatToIntBits(other.travelledDistanceLy)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (int) (this.starSystem.getId().longValue() ^ (this.starSystem.getId().longValue() >>> 32));
        result = prime * result + this.totalJumps;
        result = prime * result + Float.floatToIntBits(this.travelledDistanceLy);
        return result;
    }

    @Override
    public int compareTo(Path other) {
        int byDistance = new Float(this.getTravelledDistanceLy()).compareTo(other.getTravelledDistanceLy());
        if (byDistance != 0) {
            return byDistance;
        } else {
            int byJumps = new Integer(this.getTotalJumps()).compareTo(other.getTotalJumps());
            return byJumps;
        }
    }

    public List<Path> toSortedList() {
        List<Path> sortedPaths = new ArrayList<>();
        Path p = this;
        while (p != null) {
            sortedPaths.add(p);
            p = p.getPrev();
        }
        Collections.reverse(sortedPaths);
        return sortedPaths;
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

    public float getTravelledDistanceLy() {
        return this.travelledDistanceLy;
    }

    public void setTravelledDistanceLy(float travelledDistanceLy) {
        this.travelledDistanceLy = travelledDistanceLy;
    }

    public float getRemainingDistanceLy() {
        return this.remainingDistanceLy;
    }

    public void setRemainingDistanceLy(float remainingDistanceLy) {
        this.remainingDistanceLy = remainingDistanceLy;
    }

    public float getFuelLevel() {
        return this.fuelLevel;
    }

    public void setFuelLevel(float fuelLevel) {
        this.fuelLevel = fuelLevel;
    }

}
