package borg.edtrading.gui;

import borg.edtrading.SidePanelApp;
import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.repositories.EddbBodyRepository;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.sidepanel.TravelHistory;
import borg.edtrading.sidepanel.TravelHistoryListener;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.awt.Font;
import java.util.List;
import java.util.Locale;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;

/**
 * DiscoveryPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DiscoveryPanel extends Box implements TravelHistoryListener {

    private static final long serialVersionUID = 2933866499279397227L;

    static final Logger logger = LogManager.getLogger(DiscoveryPanel.class);

    private ApplicationContext appctx = null;
    private TravelHistory travelHistory = null;

    private JLabel closestBlackHole = new JLabel("Closest black hole: -");
    private JLabel closestNeutronStar = new JLabel("Closest neutron star: -");
    private JLabel closestEarthLikeWorld = new JLabel("Closest earth-like world: -");
    private JLabel closestAmmoniaWorld = new JLabel("Closest ammonia world: -");
    private JLabel closestWaterWorld = new JLabel("Closest water world: -");
    private JLabel closestTerraformable = new JLabel("Closest terraformable: -");

    public DiscoveryPanel(ApplicationContext appctx, TravelHistory travelHistory) {
        super(BoxLayout.Y_AXIS);

        this.appctx = appctx;
        this.travelHistory = travelHistory;
        travelHistory.addListener(this);

        Font font = new Font("Sans Serif", Font.BOLD, 24);
        if (SidePanelApp.BIG_AND_BLACK) {
            this.closestBlackHole.setFont(font);
            this.closestNeutronStar.setFont(font);
            this.closestEarthLikeWorld.setFont(font);
            this.closestAmmoniaWorld.setFont(font);
            this.closestWaterWorld.setFont(font);
            this.closestTerraformable.setFont(font);
        }

        this.add(this.closestBlackHole);
        this.add(this.closestNeutronStar);
        this.add(this.closestEarthLikeWorld);
        this.add(this.closestAmmoniaWorld);
        this.add(this.closestWaterWorld);
        this.add(this.closestTerraformable);

        this.updatePanel();
    }

    @Override
    public void onLocationChanged() {
        this.updatePanel();
    }

    private void updatePanel() {
        final Coord coord = this.travelHistory.getCoord();

        final EddbBodyRepository bodyRepo = this.appctx.getBean(EddbBodyRepository.class);

        EddbBody closestBlackHole = null;
        Float closestBlackHoleDistance = null;
        for (float range = 2; range <= 16384 && closestBlackHole == null; range *= 2) {
            float xfrom = coord.getX() - range;
            float xto = coord.getX() + range;
            float yfrom = coord.getY() - range;
            float yto = coord.getY() + range;
            float zfrom = coord.getZ() - range;
            float zto = coord.getZ() + range;

            Page<EddbBody> page = bodyRepo.findByTypeIdAndIsMainStarAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_BLACK_HOLE, Boolean.TRUE, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            while (page != null) {
                List<EddbBody> bodies = page.getContent();
                for (EddbBody body : bodies) {
                    float distance = body.getCoord().distanceTo(coord);
                    if (closestBlackHoleDistance == null || distance < closestBlackHoleDistance) {
                        closestBlackHoleDistance = distance;
                        closestBlackHole = body;
                    }
                }
                if (page.hasNext()) {
                    page = bodyRepo.findByTypeIdAndIsMainStarAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_BLACK_HOLE, Boolean.TRUE, xfrom, xto, yfrom, yto, zfrom, zto, page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestBlackHole != null) {
            this.closestBlackHole.setText(String.format(Locale.US, "Closest black hole: %s (%.1f Ly)", closestBlackHole.getName(), closestBlackHoleDistance));
        }

        EddbBody closestNeutronStar = null;
        Float closestNeutronStarDistance = null;
        for (float range = 2; range <= 16384 && closestNeutronStar == null; range *= 2) {
            float xfrom = coord.getX() - range;
            float xto = coord.getX() + range;
            float yfrom = coord.getY() - range;
            float yto = coord.getY() + range;
            float zfrom = coord.getZ() - range;
            float zto = coord.getZ() + range;

            Page<EddbBody> page = bodyRepo.findByTypeIdAndIsMainStarAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_NEUTRON_STAR, Boolean.TRUE, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            while (page != null) {
                List<EddbBody> bodies = page.getContent();
                for (EddbBody body : bodies) {
                    float distance = body.getCoord().distanceTo(coord);
                    if (closestNeutronStarDistance == null || distance < closestNeutronStarDistance) {
                        closestNeutronStarDistance = distance;
                        closestNeutronStar = body;
                    }
                }
                if (page.hasNext()) {
                    page = bodyRepo.findByTypeIdAndIsMainStarAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_NEUTRON_STAR, Boolean.TRUE, xfrom, xto, yfrom, yto, zfrom, zto, page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestNeutronStar != null) {
            this.closestNeutronStar.setText(String.format(Locale.US, "Closest neutron star: %s (%.1f Ly)", closestNeutronStar.getName(), closestNeutronStarDistance));
        }

        EddbBody closestEarthLikeWorld = null;
        Float closestEarthLikeWorldDistance = null;
        for (float range = 2; range <= 16384 && closestEarthLikeWorld == null; range *= 2) {
            float xfrom = coord.getX() - range;
            float xto = coord.getX() + range;
            float yfrom = coord.getY() - range;
            float yto = coord.getY() + range;
            float zfrom = coord.getZ() - range;
            float zto = coord.getZ() + range;

            Page<EddbBody> page = bodyRepo.findByTypeIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_EARTH_LIKE_WORLD, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            while (page != null) {
                List<EddbBody> bodies = page.getContent();
                for (EddbBody body : bodies) {
                    if (!this.travelHistory.isScanned(body.getName())) {
                        float distance = body.getCoord().distanceTo(coord);
                        if (closestEarthLikeWorldDistance == null || distance < closestEarthLikeWorldDistance) {
                            closestEarthLikeWorldDistance = distance;
                            closestEarthLikeWorld = body;
                        }
                    }
                }
                if (page.hasNext()) {
                    page = bodyRepo.findByTypeIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_EARTH_LIKE_WORLD, xfrom, xto, yfrom, yto, zfrom, zto, page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestEarthLikeWorld != null) {
            this.closestEarthLikeWorld.setText(String.format(Locale.US, "Closest earth-like world: %s (%.1f Ly)", closestEarthLikeWorld.getName(), closestEarthLikeWorldDistance));
        }

        EddbBody closestAmmoniaWorld = null;
        Float closestAmmoniaWorldDistance = null;
        for (float range = 2; range <= 16384 && closestAmmoniaWorld == null; range *= 2) {
            float xfrom = coord.getX() - range;
            float xto = coord.getX() + range;
            float yfrom = coord.getY() - range;
            float yto = coord.getY() + range;
            float zfrom = coord.getZ() - range;
            float zto = coord.getZ() + range;

            Page<EddbBody> page = bodyRepo.findByTypeIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_AMMONIA_WORLD, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            while (page != null) {
                List<EddbBody> bodies = page.getContent();
                for (EddbBody body : bodies) {
                    if (!this.travelHistory.isScanned(body.getName())) {
                        float distance = body.getCoord().distanceTo(coord);
                        if (closestAmmoniaWorldDistance == null || distance < closestAmmoniaWorldDistance) {
                            closestAmmoniaWorldDistance = distance;
                            closestAmmoniaWorld = body;
                        }
                    }
                }
                if (page.hasNext()) {
                    page = bodyRepo.findByTypeIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_AMMONIA_WORLD, xfrom, xto, yfrom, yto, zfrom, zto, page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestAmmoniaWorld != null) {
            this.closestAmmoniaWorld.setText(String.format(Locale.US, "Closest ammonia world: %s (%.1f Ly)", closestAmmoniaWorld.getName(), closestAmmoniaWorldDistance));
        }

        EddbBody closestWaterWorld = null;
        Float closestWaterWorldDistance = null;
        for (float range = 2; range <= 16384 && closestWaterWorld == null; range *= 2) {
            float xfrom = coord.getX() - range;
            float xto = coord.getX() + range;
            float yfrom = coord.getY() - range;
            float yto = coord.getY() + range;
            float zfrom = coord.getZ() - range;
            float zto = coord.getZ() + range;

            Page<EddbBody> page = bodyRepo.findByTypeIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_WATER_WORLD, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            while (page != null) {
                List<EddbBody> bodies = page.getContent();
                for (EddbBody body : bodies) {
                    if (!this.travelHistory.isScanned(body.getName())) {
                        float distance = body.getCoord().distanceTo(coord);
                        if (closestWaterWorldDistance == null || distance < closestWaterWorldDistance) {
                            closestWaterWorldDistance = distance;
                            closestWaterWorld = body;
                        }
                    }
                }
                if (page.hasNext()) {
                    page = bodyRepo.findByTypeIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_WATER_WORLD, xfrom, xto, yfrom, yto, zfrom, zto, page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestWaterWorld != null) {
            this.closestWaterWorld.setText(String.format(Locale.US, "Closest water world: %s (%.1f Ly)", closestWaterWorld.getName(), closestWaterWorldDistance));
        }

        EddbBody closestTerraformable = null;
        Float closestTerraformableDistance = null;
        for (float range = 2; range <= 16384 && closestTerraformable == null; range *= 2) {
            float xfrom = coord.getX() - range;
            float xto = coord.getX() + range;
            float yfrom = coord.getY() - range;
            float yto = coord.getY() + range;
            float zfrom = coord.getZ() - range;
            float zto = coord.getZ() + range;

            Page<EddbBody> page = bodyRepo.findByTerraformingStateIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TERRAFORMING_STATE_ID_CANDIDATE_FOR_TERRAFORMING, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            while (page != null) {
                List<EddbBody> bodies = page.getContent();
                for (EddbBody body : bodies) {
                    if (!this.travelHistory.isScanned(body.getName())) {
                        float distance = body.getCoord().distanceTo(coord);
                        if (closestTerraformableDistance == null || distance < closestTerraformableDistance) {
                            closestTerraformableDistance = distance;
                            closestTerraformable = body;
                        }
                    }
                }
                if (page.hasNext()) {
                    page = bodyRepo.findByTerraformingStateIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TERRAFORMING_STATE_ID_CANDIDATE_FOR_TERRAFORMING, xfrom, xto, yfrom, yto, zfrom, zto, page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestTerraformable != null) {
            this.closestTerraformable.setText(String.format(Locale.US, "Closest terraformable: %s (%.1f Ly)", closestTerraformable.getName(), closestTerraformableDistance));
        }
    }

    @Override
    public void onFuelLevelChanged(float newFuelLevel) {
        // Do nothing
    }

    @Override
    public void onExplorationDataSold(SellExplorationDataEntry journalEntry) {
        // Do nothing
    }

}
