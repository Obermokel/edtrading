package borg.edtrading.aystar;

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

    private final float directDistance;
    private final float maxJumpRangeBoosted;

    public LeastJumpsComparator(double directDistance, double maxJumpRangeUnboosted) {
        this.directDistance = (float) directDistance;
        this.maxJumpRangeBoosted = 4.0f * (float) maxJumpRangeUnboosted;
    }

    @Override
    public int compare(Path p1, Path p2) {
        float lyRemaining1 = p1.getRemainingDistanceLy();
        float lyRemaining2 = p2.getRemainingDistanceLy();
        float percentRemaining1 = lyRemaining1 / this.directDistance;
        float percentRemaining2 = lyRemaining2 / this.directDistance;
        int jumpsRemaining1 = (int) (lyRemaining1 / this.maxJumpRangeBoosted);
        int jumpsRemaining2 = (int) (lyRemaining2 / this.maxJumpRangeBoosted);
        int totalJumps1 = p1.getTotalJumps() + jumpsRemaining1;
        int totalJumps2 = p2.getTotalJumps() + jumpsRemaining2;
        Float rating1 = totalJumps1 + percentRemaining1;
        Float rating2 = totalJumps2 + percentRemaining2;
        //        if (p1.getStarSystem().getId() == 776L || p1.getStarSystem().getId() == 764L || p1.getStarSystem().getId() == 17072L) {
        //            logger.debug(p1.getStarSystem() + ": lyRemaining=" + lyRemaining1 + ", maxJumpRange=" + this.maxJumpRangeBoosted + ", percentRemaining=" + percentRemaining1 + ", jumpsRemaining=" + jumpsRemaining1 + ", totalJumps=" + totalJumps1 + ", rating=" + rating1);
        //        }
        //        if (p2.getStarSystem().getId() == 776L || p2.getStarSystem().getId() == 764L || p2.getStarSystem().getId() == 17072L) {
        //            logger.debug(p2.getStarSystem() + ": lyRemaining=" + lyRemaining2 + ", maxJumpRange=" + this.maxJumpRangeBoosted + ", percentRemaining=" + percentRemaining2 + ", jumpsRemaining=" + jumpsRemaining2 + ", totalJumps=" + totalJumps2 + ", rating=" + rating2);
        //        }
        return rating1.compareTo(rating2);
    }

}
