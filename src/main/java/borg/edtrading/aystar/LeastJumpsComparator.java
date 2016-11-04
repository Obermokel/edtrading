package borg.edtrading.aystar;

import borg.edtrading.data.StarSystem;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;
import java.util.Comparator;

/**
 * LeastJumpsComparator
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class LeastJumpsComparator implements Comparator<Path> {

    static final Logger logger = LogManager.getLogger(LeastJumpsComparator.class);

    private final StarSystem goal;
    private final BigDecimal directDistance;
    private final BigDecimal maxJumpRange;

    public LeastJumpsComparator(StarSystem goal, double directDistance, double ladenAndFueledBaseJumpRange) {
        this.goal = goal;
        this.directDistance = new BigDecimal(directDistance).setScale(4, BigDecimal.ROUND_HALF_UP);
        this.maxJumpRange = new BigDecimal(4.0 * ladenAndFueledBaseJumpRange).setScale(2, BigDecimal.ROUND_HALF_UP);
        //        logger.debug("directDistance=" + this.directDistance + ", maxJumpRange=" + this.maxJumpRange);
    }

    @Override
    public int compare(Path p1, Path p2) {
        BigDecimal lyRemaining1 = new BigDecimal(p1.getStarSystem().distanceTo(this.goal)).setScale(4, BigDecimal.ROUND_HALF_UP);
        BigDecimal lyRemaining2 = new BigDecimal(p2.getStarSystem().distanceTo(this.goal)).setScale(4, BigDecimal.ROUND_HALF_UP);
        BigDecimal percentRemaining1 = lyRemaining1.divide(this.directDistance, 6, BigDecimal.ROUND_HALF_UP);
        BigDecimal percentRemaining2 = lyRemaining2.divide(this.directDistance, 6, BigDecimal.ROUND_HALF_UP);
        BigDecimal jumpsRemaining1 = lyRemaining1.divide(this.maxJumpRange, 0, BigDecimal.ROUND_FLOOR);
        BigDecimal jumpsRemaining2 = lyRemaining2.divide(this.maxJumpRange, 0, BigDecimal.ROUND_FLOOR);
        BigDecimal totalJumps1 = new BigDecimal(p1.getTotalJumps()).add(jumpsRemaining1);
        BigDecimal totalJumps2 = new BigDecimal(p2.getTotalJumps()).add(jumpsRemaining2);
        BigDecimal rating1 = totalJumps1.add(percentRemaining1);
        BigDecimal rating2 = totalJumps2.add(percentRemaining2);
        //        if (p1.getStarSystem().getId() == 776L || p1.getStarSystem().getId() == 764L || p1.getStarSystem().getId() == 17072L) {
        //            logger.debug(p1.getStarSystem() + ": lyRemaining=" + lyRemaining1 + ", maxJumpRange=" + this.maxJumpRange + ", percentRemaining=" + percentRemaining1 + ", jumpsRemaining=" + jumpsRemaining1 + ", totalJumps=" + totalJumps1 + ", rating=" + rating1);
        //        }
        //        if (p2.getStarSystem().getId() == 776L || p2.getStarSystem().getId() == 764L || p2.getStarSystem().getId() == 17072L) {
        //            logger.debug(p2.getStarSystem() + ": lyRemaining=" + lyRemaining2 + ", maxJumpRange=" + this.maxJumpRange + ", percentRemaining=" + percentRemaining2 + ", jumpsRemaining=" + jumpsRemaining2 + ", totalJumps=" + totalJumps2 + ", rating=" + rating2);
        //        }
        return rating1.compareTo(rating2);
    }

}
