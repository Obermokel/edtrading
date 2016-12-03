package borg.edtrading.gui;

import borg.edtrading.SidePanelApp;
import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddb.repositories.EddbBodyRepository;
import borg.edtrading.eddb.repositories.EddbSystemRepository;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.services.EddbService;
import borg.edtrading.sidepanel.ScannedBody;
import borg.edtrading.sidepanel.TravelHistory;
import borg.edtrading.sidepanel.TravelHistoryListener;
import borg.edtrading.sidepanel.VisitedSystem;
import borg.edtrading.util.StarUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Point;
import java.util.List;
import java.util.Locale;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * DiscoveryPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DiscoveryPanel extends JPanel implements TravelHistoryListener {

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

    private Area area = null;

    public DiscoveryPanel(ApplicationContext appctx, TravelHistory travelHistory) {
        this.setLayout(new BorderLayout());

        this.appctx = appctx;
        this.travelHistory = travelHistory;
        travelHistory.addListener(this);

        Box box = new Box(BoxLayout.Y_AXIS);
        Font font = new Font("Sans Serif", Font.BOLD, 18);
        if (SidePanelApp.BIG_AND_BLACK) {
            this.closestBlackHole.setFont(font);
            this.closestNeutronStar.setFont(font);
            this.closestEarthLikeWorld.setFont(font);
            this.closestAmmoniaWorld.setFont(font);
            this.closestWaterWorld.setFont(font);
            this.closestTerraformable.setFont(font);
        }

        box.add(this.closestBlackHole);
        box.add(this.closestNeutronStar);
        box.add(this.closestEarthLikeWorld);
        box.add(this.closestAmmoniaWorld);
        box.add(this.closestWaterWorld);
        box.add(this.closestTerraformable);
        this.add(box, BorderLayout.WEST);

        this.area = new Area(appctx, travelHistory);
        this.add(this.area, BorderLayout.CENTER);

        this.updatePanel();
    }

    @Override
    public void onSystemChanged() {
        this.updatePanel();
    }

    @Override
    public void onLocationChanged() {
        // Do nothing
    }

    @Override
    public void onBodyScanned(ScannedBody scannedBody) {
        this.updatePanel();
    }

    private void updatePanel() {
        this.area.repaint();

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

    public static class Area extends JPanel {

        private static final long serialVersionUID = 8383226308842901529L;

        private ApplicationContext appctx = null;
        private TravelHistory travelHistory = null;
        private float xsize = 100f;
        private float xfrom = 0f - xsize;
        private float xto = 0f + xsize;
        private float ysize = 25f;
        private float yfrom = 0f - ysize;
        private float yto = 0f + ysize;
        private float zsize = 100f;
        private float zfrom = 0f - zsize;
        private float zto = 0f + zsize;

        public Area(ApplicationContext appctx, TravelHistory travelHistory) {
            this.appctx = appctx;
            this.travelHistory = travelHistory;
        }

        @Override
        public void paint(Graphics g) {
            super.paintComponent(g);

            // Black background
            g.setColor(new Color(20, 20, 25));
            g.fillRect(0, 0, this.getWidth(), this.getHeight());

            Coord coord = this.travelHistory.getCoord();
            xsize = 2 * 100f;
            xfrom = coord.getX() - xsize / 2;
            xto = coord.getX() + xsize / 2;
            ysize = 2 * 25f;
            yfrom = coord.getY() - ysize / 2;
            yto = coord.getY() + ysize / 2;
            zsize = ((float) this.getHeight() / (float) this.getWidth()) * xsize;
            zfrom = coord.getZ() - zsize / 2;
            zto = coord.getZ() + zsize / 2;
            EddbSystemRepository systemRepo = this.appctx.getBean(EddbSystemRepository.class);
            EddbBodyRepository bodyRepo = this.appctx.getBean(EddbBodyRepository.class);
            int psize = Math.round(this.getWidth() / 150f);
            if (psize % 2 == 0) {
                psize++;
            }
            int poffset = (psize - 1) / 2;

            Page<EddbSystem> systems = systemRepo.findByCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 10000));
            for (EddbSystem system : systems.getContent()) {
                Point p = this.coordToPoint(system.getCoord());
                float dy = Math.abs(system.getCoord().getY() - coord.getY());
                int alpha = 255 - Math.round((dy / 25f) * 192);

                g.setColor(new Color(80, 80, 80, alpha));
                g.fillRect(p.x - 1, p.y - 1, 3, 3);
            }

            Page<EddbBody> mainStars = bodyRepo.findByIsMainStarAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(Boolean.TRUE, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 10000));
            for (EddbBody mainStar : mainStars.getContent()) {
                if (StringUtils.isNotEmpty(mainStar.getSpectralClass())) {
                    Point p = this.coordToPoint(mainStar.getCoord());
                    float dy = Math.abs(mainStar.getCoord().getY() - coord.getY());
                    int alpha = 255 - Math.round((dy / 25f) * 64);

                    Color color = StarUtil.spectralClassToColor(mainStar.getSpectralClass());
                    g.setColor(new Color(color.getRed(), color.getGreen(), color.getBlue(), alpha));
                    g.fillRect(p.x - 1, p.y - 1, 3, 3);
                }
            }

            for (int i = this.travelHistory.getVisitedSystems().size() - 1; i >= 0 && i >= this.travelHistory.getVisitedSystems().size() - 1000; i--) {
                VisitedSystem visitedSystem = this.travelHistory.getVisitedSystems().get(i);

                Point p = this.coordToPoint(visitedSystem.getCoord());
                float dy = Math.abs(visitedSystem.getCoord().getY() - coord.getY());
                if (dy <= 25f) {
                    int alpha = 255 - Math.round((dy / 25f) * 192);

                    g.setColor(new Color(80, 80, 80, alpha));
                    g.fillRect(p.x - 1, p.y - 1, 3, 3);

                    for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
                        if (scannedBody.getDistanceFromArrivalLS() != null && scannedBody.getDistanceFromArrivalLS().floatValue() == 0f) {
                            alpha = 255 - Math.round((dy / 25f) * 64);

                            Color color = StarUtil.spectralClassToColor(scannedBody.getStarClass());
                            g.setColor(new Color(color.getRed(), color.getGreen(), color.getBlue(), alpha));
                            g.fillRect(p.x - 1, p.y - 1, 3, 3);
                            break;
                        }
                    }
                }
            }

            Page<EddbBody> blackHoles = bodyRepo.findByTypeIdAndIsMainStarAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_BLACK_HOLE, Boolean.TRUE, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            for (EddbBody blackHole : blackHoles.getContent()) {
                Point p = this.coordToPoint(blackHole.getCoord());

                g.setColor(new Color(80, 80, 80));
                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
                g.setColor(new Color(0, 0, 0));
                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
                g.setColor(this.travelHistory.isScanned(blackHole.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
                g.drawString(blackHole.getName(), p.x + psize, p.y + psize / 2);
            }

            Page<EddbBody> neutronStars = bodyRepo.findByTypeIdAndIsMainStarAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_NEUTRON_STAR, Boolean.TRUE, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            for (EddbBody neutronStar : neutronStars.getContent()) {
                Point p = this.coordToPoint(neutronStar.getCoord());

                g.setColor(new Color(0, 0, 160));
                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
                g.setColor(new Color(255, 255, 255));
                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
                g.setColor(this.travelHistory.isScanned(neutronStar.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
                g.drawString(neutronStar.getName(), p.x + psize, p.y + psize / 2);
            }

            Page<EddbBody> earthLikeWorlds = bodyRepo.findByTypeIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_EARTH_LIKE_WORLD, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            for (EddbBody earthLikeWorld : earthLikeWorlds.getContent()) {
                Point p = this.coordToPoint(earthLikeWorld.getCoord());

                g.setColor(new Color(0, 40, 120));
                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
                g.setColor(new Color(40, 180, 0));
                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
                g.setColor(this.travelHistory.isScanned(earthLikeWorld.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
                g.drawString(earthLikeWorld.getName(), p.x + psize, p.y + psize / 2);
            }

            Page<EddbBody> ammoniaWorlds = bodyRepo.findByTypeIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_AMMONIA_WORLD, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            for (EddbBody ammoniaWorld : ammoniaWorlds.getContent()) {
                Point p = this.coordToPoint(ammoniaWorld.getCoord());

                g.setColor(new Color(140, 140, 40));
                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
                g.setColor(new Color(120, 120, 0));
                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
                g.setColor(this.travelHistory.isScanned(ammoniaWorld.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
                g.drawString(ammoniaWorld.getName(), p.x + psize, p.y + psize / 2);
            }

            Page<EddbBody> waterWorlds = bodyRepo.findByTypeIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TYPE_ID_WATER_WORLD, xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 1000));
            for (EddbBody waterWorld : waterWorlds.getContent()) {
                Point p = this.coordToPoint(waterWorld.getCoord());

                g.setColor(new Color(0, 0, 120));
                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
                g.setColor(new Color(0, 0, 80));
                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
                g.setColor(this.travelHistory.isScanned(waterWorld.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
                g.drawString(waterWorld.getName(), p.x + psize, p.y + psize / 2);
            }

            Page<EddbBody> terraformingCandidates = bodyRepo.findByTerraformingStateIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(EddbBody.TERRAFORMING_STATE_ID_CANDIDATE_FOR_TERRAFORMING, xfrom, xto, yfrom, yto, zfrom, zto,
                    new PageRequest(0, 1000));
            for (EddbBody terraformingCandidate : terraformingCandidates.getContent()) {
                Point p = this.coordToPoint(terraformingCandidate.getCoord());

                g.setColor(new Color(0, 160, 20));
                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
                g.setColor(new Color(200, 120, 50));
                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
                g.setColor(this.travelHistory.isScanned(terraformingCandidate.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
                g.drawString(terraformingCandidate.getName(), p.x + psize, p.y + psize / 2);
            }

            // Me
            g.setColor(Color.ORANGE);
            try {
                EddbSystem system = this.appctx.getBean(EddbService.class).searchSystemByName(this.travelHistory.getSystemName());
                if (system != null) {
                    Page<EddbBody> bodies = bodyRepo.findBySystemId(system.getId(), new PageRequest(0, 250));
                    g.setColor(Color.RED);
                    for (EddbBody body : bodies.getContent()) {
                        if (Boolean.TRUE.equals(body.getIsMainStar())) {
                            if (StringUtils.isNotEmpty(body.getSpectralClass())) {
                                g.setColor(Color.GREEN);
                            }
                            break;
                        }
                    }
                }
            } catch (BeansException e) {
                logger.error("Failed to find current system '" + this.travelHistory.getSystemName() + "'", e);
            }
            if (!g.getColor().equals(Color.GREEN)) {
                for (int i = this.travelHistory.getVisitedSystems().size() - 1; !g.getColor().equals(Color.GREEN) && i >= 0 && i >= this.travelHistory.getVisitedSystems().size() - 1000; i--) {
                    VisitedSystem visitedSystem = this.travelHistory.getVisitedSystems().get(i);
                    if (visitedSystem.getCoord().distanceTo(coord) <= 0.01f) {
                        for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
                            if (scannedBody.getDistanceFromArrivalLS() != null && scannedBody.getDistanceFromArrivalLS().floatValue() == 0f) {
                                g.setColor(Color.GREEN);
                                break;
                            }
                        }
                        break;
                    }
                }
            }
            Point p = this.coordToPoint(coord);
            g.fillRect(p.x - poffset, p.y - poffset, psize, psize);
        }

        private Point coordToPoint(Coord coord) {
            float xPercent = (coord.getX() - this.xfrom) / this.xsize;
            float yPercent = 1.0f - ((coord.getZ() - this.zfrom) / this.zsize);

            return new Point(Math.round(xPercent * this.getWidth()), Math.round(yPercent * this.getHeight()));
        }

    }

}
