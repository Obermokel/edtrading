package borg.edtrading;

import borg.edtrading.cfg.Config;
import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddb.repositories.EddbBodyRepository;
import borg.edtrading.eddb.repositories.EddbSystemRepository;
import borg.edtrading.eddn.EddnListener;
import borg.edtrading.eddn.EddnReaderThread;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.AbstractJournalEntry.Faction;
import borg.edtrading.journal.entries.exploration.ScanEntry;
import borg.edtrading.journal.entries.location.FSDJumpEntry;
import borg.edtrading.services.EddbService;
import borg.edtrading.util.MiscUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.domain.Sort.NullHandling;

import java.io.IOException;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

/**
 * EddbUpdate
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbUpdate {

    static final Logger logger = LogManager.getLogger(EddbUpdate.class);

    private static final AnnotationConfigApplicationContext APPCTX = new AnnotationConfigApplicationContext(Config.class);

    public static void main(String[] args) throws IOException {
        EddbUpdateThread eddbUpdateThread = new EddbUpdateThread();
        eddbUpdateThread.start();

        EddnReaderThread eddnReaderThread = new EddnReaderThread();
        eddnReaderThread.addListener(new ElasticUpdater(APPCTX));
        eddnReaderThread.start();
    }

    public static class EddbUpdateThread extends Thread {

        private EddbService eddbService = null;

        public EddbUpdateThread() {
            this.setName("EddbUpdateThread");
            this.setDaemon(false);

            this.eddbService = APPCTX.getBean(EddbService.class);
        }

        @Override
        public void run() {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    this.eddbService.updateEddbData(/* forceReindex = */ false, /* deleteOldEntities = */ true);

                    Thread.sleep(30 * 60 * 1000L);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        }

    }

    public static class ElasticUpdater implements EddnListener {

        private EddbSystemRepository eddbSystemRepository = null;
        private LinkedList<String> lastSystems = new LinkedList<>();
        private EddbBodyRepository eddbBodyRepository = null;
        private LinkedList<String> lastBodies = new LinkedList<>();

        public ElasticUpdater(ApplicationContext appctx) {
            this.eddbSystemRepository = appctx.getBean(EddbSystemRepository.class);
            this.eddbBodyRepository = appctx.getBean(EddbBodyRepository.class);
        }

        @Override
        public void onCommanderLocation(Date timestamp, String commanderName, String systemName, Coord systemCoords, List<Faction> systemFactions) {
            // Do nothing
        }

        @Override
        public void onJournalData(AbstractJournalEntry journalData) {
            if (journalData.getEvent() == Event.FSDJump) {
                this.onFsdJump((FSDJumpEntry) journalData);
            } else if (journalData.getEvent() == Event.Scan) {
                this.onScan((ScanEntry) journalData);
            }
        }

        private void onFsdJump(FSDJumpEntry journalData) {
            if (this.lastSystems.contains(journalData.getStarSystem())) {
                return;
            } else {
                this.lastSystems.addLast(journalData.getStarSystem());
                while (this.lastSystems.size() > 100) {
                    this.lastSystems.removeFirst();
                }
            }

            if (journalData.getStarPos() != null) {
                float x = journalData.getStarPos().getX();
                float y = journalData.getStarPos().getY();
                float z = journalData.getStarPos().getZ();

                Page<EddbSystem> systemsPage = this.eddbSystemRepository.findByCoordWithin(x - 0.01f, x + 0.01f, y - 0.01f, y + 0.01f, z - 0.01f, z + 0.01f, new PageRequest(0, 10));

                if (systemsPage.getTotalElements() > 1) {
                    logger.warn("Found " + systemsPage.getTotalElements() + " systems for " + journalData.getStarPos() + ": " + systemsPage.getContent());
                } else if (systemsPage.getTotalElements() == 1) {
                    // Already existent
                } else {
                    // New star system
                    logger.debug("New system '" + journalData.getStarSystem() + "' @ " + journalData.getStarPos());
                    Page<EddbSystem> lastSystem = this.eddbSystemRepository.findAll(new PageRequest(0, 1, new Sort(new Sort.Order(Direction.DESC, "id", NullHandling.NULLS_LAST))));
                    long lastId = lastSystem == null || lastSystem.getTotalElements() < 1 ? 0 : lastSystem.getContent().get(0).getId();

                    EddbSystem entity = new EddbSystem();
                    entity.setId(lastId + 1);
                    entity.setCreatedAt(new Date(0));
                    entity.setUpdatedAt(new Date(0));
                    entity.setName(journalData.getStarSystem());
                    entity.setX(x);
                    entity.setY(y);
                    entity.setZ(z);

                    this.eddbSystemRepository.save(entity);
                }
            }
        }

        private void onScan(ScanEntry journalData) {
            if (this.lastBodies.contains(journalData.getBodyName())) {
                return;
            } else {
                this.lastBodies.addLast(journalData.getBodyName());
                while (this.lastBodies.size() > 100) {
                    this.lastBodies.removeFirst();
                }
            }

            if (journalData.getStarPos() != null) {
                float x = journalData.getStarPos().getX();
                float y = journalData.getStarPos().getY();
                float z = journalData.getStarPos().getZ();

                Page<EddbSystem> systemsPage = this.eddbSystemRepository.findByCoordWithin(x - 0.01f, x + 0.01f, y - 0.01f, y + 0.01f, z - 0.01f, z + 0.01f, new PageRequest(0, 10));

                if (systemsPage.getTotalElements() != 1) {
                    logger.warn("Found " + systemsPage.getTotalElements() + " systems for " + journalData.getStarPos() + ": " + systemsPage.getContent());
                } else {
                    Long systemId = systemsPage.getContent().get(0).getId();

                    Page<EddbBody> bodiesPage = this.eddbBodyRepository.findByName(journalData.getBodyName(), new PageRequest(0, 10));

                    if (bodiesPage.getTotalElements() > 1) {
                        logger.warn("Found " + bodiesPage.getTotalElements() + " bodies for '" + journalData.getBodyName() + "': " + bodiesPage.getContent());
                    } else if (bodiesPage.getTotalElements() == 1) {
                        // Already existent
                    } else {
                        // New body
                        logger.debug("New body '" + journalData.getBodyName() + "' @ " + journalData.getStarPos());
                        Page<EddbBody> lastBody = this.eddbBodyRepository.findAll(new PageRequest(0, 1, new Sort(new Sort.Order(Direction.DESC, "id", NullHandling.NULLS_LAST))));
                        long lastId = lastBody == null || lastBody.getTotalElements() < 1 ? 0 : lastBody.getContent().get(0).getId();

                        EddbBody entity = new EddbBody();
                        entity.setId(lastId + 1);
                        entity.setSystemId(systemId);
                        entity.setCreatedAt(new Date(0));
                        entity.setUpdatedAt(new Date(0));
                        entity.setName(journalData.getBodyName());
                        entity.setCoord(journalData.getStarPos());
                        entity.setDistanceToArrival(MiscUtil.getAsBigDecimal(journalData.getDistanceFromArrivalLS()));
                        entity.setIsMainStar(journalData.getDistanceFromArrivalLS() != null && journalData.getDistanceFromArrivalLS().floatValue() == 0 && StringUtils.isNotBlank(journalData.getStarType()));
                        if ("H".equals(journalData.getStarType()) || "N".equals(journalData.getStarType()) || "DA".equals(journalData.getStarType()) || "DB".equals(journalData.getStarType()) || "DC".equals(journalData.getStarType())) {
                            entity.setGroupId(1L);
                            entity.setGroupName("Compact star");
                        } else if (StringUtils.isNotBlank(journalData.getStarType())) {
                            entity.setGroupId(2L);
                            entity.setGroupName("Star");
                        } else if (journalData.getBodyName().toLowerCase().endsWith(" belt")) {
                            entity.setGroupId(3L);
                            entity.setGroupName("Belt");
                        } else {
                            entity.setGroupId(6L);
                            entity.setGroupName("Planet");
                        }
                        if ("H".equals(journalData.getStarType())) {
                            entity.setTypeId(EddbBody.TYPE_ID_BLACK_HOLE);
                        } else if ("N".equals(journalData.getStarType())) {
                            entity.setTypeId(EddbBody.TYPE_ID_NEUTRON_STAR);
                        } else if ("Earth-like world".equals(journalData.getPlanetClass())) {
                            entity.setTypeId(EddbBody.TYPE_ID_EARTH_LIKE_WORLD);
                        } else if ("Ammonia world".equals(journalData.getPlanetClass())) {
                            entity.setTypeId(EddbBody.TYPE_ID_AMMONIA_WORLD);
                        } else if ("Water world".equals(journalData.getPlanetClass())) {
                            entity.setTypeId(EddbBody.TYPE_ID_WATER_WORLD);
                        } else {
                            entity.setTypeId(null);
                        }
                        entity.setTypeName(journalData.getPlanetClass());
                        entity.setSpectralClass(journalData.getStarType());
                        entity.setStarClass(entity.toStarClass());
                        if ("".equals(journalData.getTerraformState())) {
                            entity.setTerraformingStateId(EddbBody.TERRAFORMING_STATE_ID_NOT_TERRAFORMABLE);
                            entity.setTerraformingStateName("Not terraformable");
                        } else if ("Terraformable".equals(journalData.getTerraformState())) {
                            entity.setTerraformingStateId(EddbBody.TERRAFORMING_STATE_ID_CANDIDATE_FOR_TERRAFORMING);
                            entity.setTerraformingStateName("Candidate for terraforming");
                        } else {
                            entity.setTerraformingStateId(null);
                            entity.setTerraformingStateName(null);
                        }

                        this.eddbBodyRepository.save(entity);
                    }
                }
            }
        }

    }

}
