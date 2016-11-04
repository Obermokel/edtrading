package borg.edtrading.aystar;

import borg.edtrading.data.Coord;
import borg.edtrading.data.StarSystem;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * AyStar
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class AyStar {

    static final Logger logger = LogManager.getLogger(AyStar.class);

    private static final int SECTOR_SIZE = 50; // ly

    private PriorityQueue<Path> open = null;
    private Set<StarSystem> closed = null;
    private StarSystem source = null;
    private StarSystem goal = null;
    private Set<StarSystem> starSystemsWithNeutronStars = null;
    private Set<StarSystem> starSystemsWithScoopableStars = null;
    private Map<Coord, List<StarSystem>> starSystemsWithScoopableStarsBySector = null;
    private double ladenAndFueledBaseJumpRange = 0;
    private int maxJumpsWithoutScooping = 0;

    public void initialize(StarSystem source, StarSystem goal, Set<StarSystem> starSystemsWithNeutronStars, Set<StarSystem> starSystemsWithScoopableStars, double ladenAndFueledBaseJumpRange, int maxJumpsWithoutScooping) {
        if (!starSystemsWithNeutronStars.contains(goal) && !starSystemsWithScoopableStars.contains(goal)) {
            throw new IllegalArgumentException("goal not in useable star systems");
        } else {
            this.ladenAndFueledBaseJumpRange = ladenAndFueledBaseJumpRange;
            this.maxJumpsWithoutScooping = maxJumpsWithoutScooping;
            this.open = new PriorityQueue<>(new LeastJumpsComparator(goal, source.distanceTo(goal), this.ladenAndFueledBaseJumpRange));
            this.closed = new HashSet<>();
            this.source = source;
            this.goal = goal;
            this.starSystemsWithNeutronStars = starSystemsWithNeutronStars;
            this.starSystemsWithScoopableStars = starSystemsWithScoopableStars;
            this.starSystemsWithScoopableStarsBySector = mapBySector(starSystemsWithScoopableStars);

            this.open.add(new Path(source));
        }
    }

    public Path findPath() {
        while (this.open.size() > 0) {
            Path path = this.open.poll();

            if (this.closed.contains(path.getStarSystem())) {
                // We already found a better path
                continue;
            } else {
                // Because we always poll the best path so far, the current path is
                // the best path to this station
                this.closed.add(path.getStarSystem());
            }

            if (this.closed.size() % 10000 == 0 && logger.isDebugEnabled()) {
                logger.debug(String.format("Open: %,15d / Closed: %,15d || %d jump(s), %.0f Ly", this.open.size(), this.closed.size(), path.getTotalJumps(), path.getTotalDistanceLy()));
            }

            if (path.getStarSystem().equals(this.goal)) {
                // We reached our destination
                return path;
            }

            List<StarSystem> neighbours = this.findNeighbours(path);

            for (StarSystem neighbour : neighbours) {
                if (!this.closed.contains(neighbour)) {
                    double extraDistanceLy = path.getStarSystem().distanceTo(neighbour);
                    Path newPath = new Path(path, neighbour, extraDistanceLy);
                    //if (newPath.getTotalDistanceLy() <= this.maxTotalDistanceLy) {
                    this.open.offer(newPath);
                    //}
                }
            }

            //            if (this.open.size() > 2 * 5000000) {
            //                List<Path> temp = new ArrayList<>(5000000);
            //                for (int i = 0; i < 5000000; i++) {
            //                    temp.add(this.open.poll());
            //                }
            //                this.open.clear();
            //                this.open.addAll(temp);
            //            }
        }

        return null;
    }

    private List<StarSystem> findNeighbours(Path path) {
        final StarSystem currentStarSystem = path.getStarSystem();
        final Coord currentCoord = currentStarSystem.getCoord();
        final double currentDistanceToGoal = currentStarSystem.distanceTo(this.goal);

        // Do we have an overcharged FSD?
        final double currentJumpRange = this.starSystemsWithNeutronStars.contains(currentStarSystem) ? 4 * ladenAndFueledBaseJumpRange : ladenAndFueledBaseJumpRange;
        //final double currentJumpRangeManhattan = 1.5 * currentJumpRange;

        // Do we need to scoop?
        int jumpsWithoutScooping = 0;
        Path p = path;
        while (p.getPrev() != null) {
            if (this.starSystemsWithScoopableStars.contains(p.getStarSystem())) {
                break; // Found the last scoopable system
            } else {
                jumpsWithoutScooping++; // Current system is not scoopable and we have jumped here, so +1 jump w/o scooping
            }
            p = p.getPrev();
        }
        boolean mustScoop = jumpsWithoutScooping >= this.maxJumpsWithoutScooping;

        // Find reachable systems
        List<StarSystem> systemsInRange = new ArrayList<>();
        if (!mustScoop) {
            List<StarSystem> neutronInRange = this.starSystemsWithNeutronStars.stream().filter(st -> st.distanceTo(currentStarSystem) <= currentJumpRange).collect(Collectors.toList());
            //logger.debug("Considering " + neutronInRange.size() + " neutrons in range: " + neutronInRange);
            systemsInRange.addAll(neutronInRange);
        }
        List<StarSystem> scoopableSystemsInCloseSectors = findSystemsBySector(this.starSystemsWithScoopableStarsBySector, currentCoord, currentJumpRange);
        systemsInRange.addAll(scoopableSystemsInCloseSectors.stream().filter(st -> st.distanceTo(currentStarSystem) <= currentJumpRange /*&& st.distanceTo(goal) < currentDistanceToGoal*/).collect(Collectors.toList()));

        // Keep only those which bring us closer to the goal, i.e. the new system is closer to the goal than our current distance to the goal
        //Set<StarSystem> systemsInTravelDirection = systemsInRange.stream().filter(st -> st.distanceTo(goal) < currentDistanceToGoal).collect(Collectors.toSet());

        // Finished
        return systemsInRange;
    }

    private static List<StarSystem> findSystemsBySector(Map<Coord, List<StarSystem>> systemsBySector, Coord currentCoord, double currentJumpRange) {
        List<StarSystem> result = new ArrayList<>();

        Coord currentSector = coordToSector(currentCoord);
        int nSideSectors = (int) Math.ceil(currentJumpRange / SECTOR_SIZE);
        for (int x = -nSideSectors; x <= nSideSectors; x++) {
            for (int y = -nSideSectors; y <= nSideSectors; y++) {
                for (int z = -nSideSectors; z <= nSideSectors; z++) {
                    Coord sector = new Coord(currentSector.getX() + x * SECTOR_SIZE, currentSector.getY() + y * SECTOR_SIZE, currentSector.getZ() + z * SECTOR_SIZE);
                    List<StarSystem> systems = systemsBySector.get(sector);
                    if (systems != null) {
                        result.addAll(systems);
                    }
                }
            }
        }
        //logger.debug("Found " + result.size() + " systems around sector " + currentSector + " with jump range = " + currentJumpRange);

        return result;
    }

    private Map<Coord, List<StarSystem>> mapBySector(Collection<StarSystem> starSystems) {
        Map<Coord, List<StarSystem>> result = new HashMap<>();
        for (StarSystem starSystem : starSystems) {
            Coord sector = coordToSector(starSystem.getCoord());
            List<StarSystem> systemsInThisSector = result.get(sector);
            if (systemsInThisSector == null) {
                systemsInThisSector = new ArrayList<>();
                result.put(sector, systemsInThisSector);
            }
            systemsInThisSector.add(starSystem);
        }
        logger.debug("Mapped " + starSystems.size() + " systems into " + result.size() + " sectors");
        return result;
    }

    private static Coord coordToSector(Coord coord) {
        return new Coord(((int) coord.getX() / SECTOR_SIZE) * SECTOR_SIZE, ((int) coord.getY() / SECTOR_SIZE) * SECTOR_SIZE, ((int) coord.getZ() / SECTOR_SIZE) * SECTOR_SIZE);
    }

}
