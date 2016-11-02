package borg.edtrading.aystar;

import borg.edtrading.data.StarSystem;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Comparator;

/**
 * LeastJumpsComparator
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class LeastJumpsComparator implements Comparator<Path> {

    static final Logger logger = LogManager.getLogger(LeastJumpsComparator.class);

    private final StarSystem goal;
    private final double avgJumpRange;

    public LeastJumpsComparator(StarSystem goal, double avgJumpRange) {
        this.goal = goal;
        this.avgJumpRange = avgJumpRange;
    }

    @Override
    public int compare(Path p1, Path p2) {
        double lyRemaining1 = p1.getStarSystem().distanceTo(this.goal);
        double lyRemaining2 = p2.getStarSystem().distanceTo(this.goal);
        int jumpsRemaining1 = (int) lyRemaining1; // 1 jump per ly - bad estimation, but oh well
        int jumpsRemaining2 = (int) lyRemaining2; // 1 jump per ly - bad estimation, but oh well
        Integer totalJumps1 = p1.getTotalJumps() + jumpsRemaining1;
        Integer totalJumps2 = p2.getTotalJumps() + jumpsRemaining2;
        return totalJumps1.compareTo(totalJumps2);
    }

}
