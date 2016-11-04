package borg.edtrading.data;

import java.io.Serializable;

/**
 * Coord
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Coord implements Serializable {

    private static final long serialVersionUID = 1052805326828839956L;

    private final float x;
    private final float y;
    private final float z;

    public Coord(float x, float y, float z) {
        this.x = x;
        this.y = y;
        this.z = z;
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
        Coord other = (Coord) obj;
        if (Float.floatToIntBits(this.x) != Float.floatToIntBits(other.x)) {
            return false;
        }
        if (Float.floatToIntBits(this.y) != Float.floatToIntBits(other.y)) {
            return false;
        }
        if (Float.floatToIntBits(this.z) != Float.floatToIntBits(other.z)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Float.floatToIntBits(this.x);
        result = prime * result + Float.floatToIntBits(this.y);
        result = prime * result + Float.floatToIntBits(this.z);
        return result;
    }

    @Override
    public String toString() {
        return String.format("%.0f:%.0f:%.0f", x, y, z);
    }

    public float distanceTo(Coord other) {
        float dx = this.x - other.x;
        float dy = this.y - other.y;
        float dz = this.z - other.z;

        return (float) Math.sqrt(dx * dx + dy * dy + dz * dz);
    }

    //    public float distanceManhattanTo(Coord other) {
    //        float dx = this.x - other.x;
    //        float dy = this.y - other.y;
    //        float dz = this.z - other.z;
    //
    //        return Math.abs(dx) + Math.abs(dy) + Math.abs(dz);
    //    }

    public float getX() {
        return this.x;
    }

    public float getY() {
        return this.y;
    }

    public float getZ() {
        return this.z;
    }

}
