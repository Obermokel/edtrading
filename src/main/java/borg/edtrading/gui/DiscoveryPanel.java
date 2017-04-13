package borg.edtrading.gui;

import borg.edtrading.SidePanelApp;
import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddn.EddnListener;
import borg.edtrading.journal.entries.AbstractJournalEntry.Faction;
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
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Point;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * DiscoveryPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DiscoveryPanel extends JPanel implements TravelHistoryListener, EddnListener {

    private static final long serialVersionUID = 2933866499279397227L;

    static final Logger logger = LogManager.getLogger(DiscoveryPanel.class);

    private ApplicationContext appctx = null;
    private TravelHistory travelHistory = null;

    private JTextField txtClosestBlackHoleName = new JTextField(30);
    private JLabel lblClosestBlackHoleDistance = new JLabel();
    private JTextField txtClosestNeutronStarName = new JTextField(30);
    private JLabel lblClosestNeutronStarDistance = new JLabel();
    private JTextField txtClosestEarthLikeWorldName = new JTextField(30);
    private JLabel lblClosestEarthLikeWorldDistance = new JLabel();
    private JTextField txtClosestAmmoniaWorldName = new JTextField(30);
    private JLabel lblClosestAmmoniaWorldDistance = new JLabel();
    private JTextField txtClosestWaterWorldName = new JTextField(30);
    private JLabel lblClosestWaterWorldDistance = new JLabel();
    private JTextField txtClosestTerraformableName = new JTextField(30);
    private JLabel lblClosestTerraformableDistance = new JLabel();

    private Area area = null;

    public DiscoveryPanel(ApplicationContext appctx, TravelHistory travelHistory) {
        this.setLayout(new BorderLayout());

        this.appctx = appctx;
        this.travelHistory = travelHistory;
        travelHistory.addListener(this);

        Box box = new Box(BoxLayout.Y_AXIS);
        Font font = new Font("Sans Serif", Font.BOLD, 18);
        if (SidePanelApp.BIG_AND_BLACK) {
            this.txtClosestBlackHoleName.setFont(font);
            this.lblClosestBlackHoleDistance.setFont(font);
            this.txtClosestNeutronStarName.setFont(font);
            this.lblClosestNeutronStarDistance.setFont(font);
            this.txtClosestEarthLikeWorldName.setFont(font);
            this.lblClosestEarthLikeWorldDistance.setFont(font);
            this.txtClosestAmmoniaWorldName.setFont(font);
            this.lblClosestAmmoniaWorldDistance.setFont(font);
            this.txtClosestWaterWorldName.setFont(font);
            this.lblClosestWaterWorldDistance.setFont(font);
            this.txtClosestTerraformableName.setFont(font);
            this.lblClosestTerraformableDistance.setFont(font);
        }

        box.add(distanceLabel("Closest black hole:", this.lblClosestBlackHoleDistance));
        box.add(this.txtClosestBlackHoleName);
        box.add(new JLabel(" "));
        box.add(distanceLabel("Closest neutron star:", this.lblClosestNeutronStarDistance));
        box.add(this.txtClosestNeutronStarName);
        box.add(new JLabel(" "));
        box.add(distanceLabel("Closest earth-like world:", this.lblClosestEarthLikeWorldDistance));
        box.add(this.txtClosestEarthLikeWorldName);
        box.add(new JLabel(" "));
        box.add(distanceLabel("Closest ammonia world:", this.lblClosestAmmoniaWorldDistance));
        box.add(this.txtClosestAmmoniaWorldName);
        box.add(new JLabel(" "));
        box.add(distanceLabel("Closest water world:", this.lblClosestWaterWorldDistance));
        box.add(this.txtClosestWaterWorldName);
        box.add(new JLabel(" "));
        box.add(distanceLabel("Closest terraformable:", this.lblClosestTerraformableDistance));
        box.add(this.txtClosestTerraformableName);
        box.add(new JLabel(" "));
        JPanel dummyPanel = new JPanel(new BorderLayout());
        dummyPanel.add(box, BorderLayout.NORTH);
        dummyPanel.add(new JLabel(""), BorderLayout.CENTER);
        this.add(dummyPanel, BorderLayout.WEST);

        this.area = new Area(appctx, travelHistory);
        this.add(this.area, BorderLayout.CENTER);

        this.updatePanel();
    }

    private JPanel distanceLabel(String label, JLabel lblDistance) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 5));
        JLabel lbl = new JLabel(label);
        if (SidePanelApp.BIG_AND_BLACK) {
            lbl.setFont(new Font("Sans Serif", Font.BOLD, 18));
            lblDistance.setFont(new Font("Sans Serif", Font.BOLD, 18));
        }
        panel.add(lbl);
        panel.add(lblDistance);
        return panel;
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

        final EddbService eddbService = this.appctx.getBean(EddbService.class);

        EddbBody closestBlackHole = null;
        Float closestBlackHoleDistance = null;
        for (float range = 2; range <= 16384 && closestBlackHole == null; range *= 2) {
            Page<EddbBody> page = eddbService.findStarsNear(coord, range, /* isMainStar = */ Boolean.TRUE, Arrays.asList("BH", "SMBH"), new PageRequest(0, 1000));
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
                    page = eddbService.findStarsNear(coord, range, /* isMainStar = */ Boolean.TRUE, Arrays.asList("BH", "SMBH"), page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestBlackHole != null) {
            this.txtClosestBlackHoleName.setText(String.format(Locale.US, "%s", closestBlackHole.getName()));
            this.lblClosestBlackHoleDistance.setText(String.format(Locale.US, "%.1f Ly", closestBlackHoleDistance));
        }

        EddbBody closestNeutronStar = null;
        Float closestNeutronStarDistance = null;
        for (float range = 2; range <= 16384 && closestNeutronStar == null; range *= 2) {
            Page<EddbBody> page = eddbService.findStarsNear(coord, range, /* isMainStar = */ Boolean.TRUE, Arrays.asList("NS"), new PageRequest(0, 1000));
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
                    page = eddbService.findStarsNear(coord, range, /* isMainStar = */ Boolean.TRUE, Arrays.asList("NS"), page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestNeutronStar != null) {
            this.txtClosestNeutronStarName.setText(String.format(Locale.US, "%s", closestNeutronStar.getName()));
            this.lblClosestNeutronStarDistance.setText(String.format(Locale.US, "%.1f Ly", closestNeutronStarDistance));
        }

        EddbBody closestEarthLikeWorld = null;
        Float closestEarthLikeWorldDistance = null;
        for (float range = 2; range <= 16384 && closestEarthLikeWorld == null; range *= 2) {
            Page<EddbBody> page = eddbService.findPlanetsNear(coord, range, /* isTerraformingCandidate = */ null, Arrays.asList(EddbBody.TYPE_ID_EARTH_LIKE_WORLD), new PageRequest(0, 1000));
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
                    page = eddbService.findPlanetsNear(coord, range, /* isTerraformingCandidate = */ null, Arrays.asList(EddbBody.TYPE_ID_EARTH_LIKE_WORLD), page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestEarthLikeWorld != null) {
            this.txtClosestEarthLikeWorldName.setText(String.format(Locale.US, "%s", closestEarthLikeWorld.getName()));
            this.lblClosestEarthLikeWorldDistance.setText(String.format(Locale.US, "%.1f Ly", closestEarthLikeWorldDistance));
        }

        EddbBody closestAmmoniaWorld = null;
        Float closestAmmoniaWorldDistance = null;
        for (float range = 2; range <= 16384 && closestAmmoniaWorld == null; range *= 2) {
            Page<EddbBody> page = eddbService.findPlanetsNear(coord, range, /* isTerraformingCandidate = */ null, Arrays.asList(EddbBody.TYPE_ID_AMMONIA_WORLD), new PageRequest(0, 1000));
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
                    page = eddbService.findPlanetsNear(coord, range, /* isTerraformingCandidate = */ null, Arrays.asList(EddbBody.TYPE_ID_AMMONIA_WORLD), page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestAmmoniaWorld != null) {
            this.txtClosestAmmoniaWorldName.setText(String.format(Locale.US, "%s", closestAmmoniaWorld.getName()));
            this.lblClosestAmmoniaWorldDistance.setText(String.format(Locale.US, "%.1f Ly", closestAmmoniaWorldDistance));
        }

        EddbBody closestWaterWorld = null;
        Float closestWaterWorldDistance = null;
        for (float range = 2; range <= 16384 && closestWaterWorld == null; range *= 2) {
            Page<EddbBody> page = eddbService.findPlanetsNear(coord, range, /* isTerraformingCandidate = */ null, Arrays.asList(EddbBody.TYPE_ID_WATER_WORLD), new PageRequest(0, 1000));
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
                    page = eddbService.findPlanetsNear(coord, range, /* isTerraformingCandidate = */ null, Arrays.asList(EddbBody.TYPE_ID_WATER_WORLD), page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestWaterWorld != null) {
            this.txtClosestWaterWorldName.setText(String.format(Locale.US, "%s", closestWaterWorld.getName()));
            this.lblClosestWaterWorldDistance.setText(String.format(Locale.US, "%.1f Ly", closestWaterWorldDistance));
        }

        EddbBody closestTerraformable = null;
        Float closestTerraformableDistance = null;
        for (float range = 2; range <= 16384 && closestTerraformable == null; range *= 2) {
            Page<EddbBody> page = eddbService.findPlanetsNear(coord, range, /* isTerraformingCandidate = */ Boolean.TRUE, null, new PageRequest(0, 1000));
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
                    page = eddbService.findPlanetsNear(coord, range, /* isTerraformingCandidate = */ Boolean.TRUE, null, page.nextPageable());
                } else {
                    page = null;
                }
            }
        }
        if (closestTerraformable != null) {
            this.txtClosestTerraformableName.setText(String.format(Locale.US, "%s", closestTerraformable.getName()));
            this.lblClosestTerraformableDistance.setText(String.format(Locale.US, "%.1f Ly", closestTerraformableDistance));
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

    @Override
    public void onCommanderLocation(Date timestamp, String commanderName, String systemName, Coord systemCoords, List<Faction> systemFactions) {
        this.area.onCommanderLocation(timestamp, commanderName, systemName, systemCoords, systemFactions);
        this.area.repaint();
    }

    public static class Area extends JPanel implements EddnListener {

        private static final long serialVersionUID = 8383226308842901529L;

        private LinkedHashMap<String, Coord> commanderLocations = new LinkedHashMap<>(10000);

        private ApplicationContext appctx = null;
        private TravelHistory travelHistory = null;
        private float xsize = 25f;
        private float xfrom = 0f - xsize;
        private float xto = 0f + xsize;
        private float ysize = 25f;
        private float yfrom = 0f - ysize;
        private float yto = 0f + ysize;
        private float zsize = 25f;
        private float zfrom = 0f - zsize;
        private float zto = 0f + zsize;

        public Area(ApplicationContext appctx, TravelHistory travelHistory) {
            this.appctx = appctx;
            this.travelHistory = travelHistory;
        }

        @Override
        public void onCommanderLocation(Date timestamp, String commanderName, String systemName, Coord systemCoords, List<Faction> systemFactions) {
            this.commanderLocations.put(commanderName, systemCoords);
        }

        @Override
        public void paint(Graphics g) {
            super.paintComponent(g);

            // Black background
            g.setColor(new Color(20, 20, 25));
            g.fillRect(0, 0, this.getWidth(), this.getHeight());

            Coord coord = this.travelHistory.getCoord();
            //zsize = ((float) this.getHeight() / (float) this.getWidth()) * xsize;
            zsize = 2 * 100f;
            zfrom = coord.getZ() - zsize / 2;
            zto = coord.getZ() + zsize / 2;
            //xsize = 2 * 160f;
            xsize = ((float) this.getWidth() / (float) this.getHeight()) * zsize;
            xfrom = coord.getX() - xsize / 2;
            xto = coord.getX() + xsize / 2;
            //ysize = xsize / 4;
            //ysize = zsize / 4;
            ysize = Math.min(xsize, zsize);
            yfrom = coord.getY() - ysize / 2;
            yto = coord.getY() + ysize / 2;
            //            EddbSystemRepository systemRepo = this.appctx.getBean(EddbSystemRepository.class);
            //            EddbBodyRepository bodyRepo = this.appctx.getBean(EddbBodyRepository.class);
            int psize = Math.round(this.getWidth() / 150f);
            if (psize % 2 == 0) {
                psize++;
            }
            int poffset = (psize - 1) / 2;
            EddbService eddbService = this.appctx.getBean(EddbService.class);

            Page<EddbSystem> systems = eddbService.findSystemsWithin(xfrom, xto, yfrom, yto, zfrom, zto, new PageRequest(0, 10000));
            for (EddbSystem system : systems.getContent()) {
                Point p = this.coordToPoint(system.getCoord());
                float dy = Math.abs(system.getCoord().getY() - coord.getY());
                int alpha = 255 - Math.round((dy / (ysize / 2)) * 255);

                g.setColor(new Color(80, 80, 80, alpha));
                g.fillRect(p.x - 1, p.y - 1, 3, 3);
            }

            Page<EddbBody> mainStars = eddbService.findStarsWithin(xfrom, xto, yfrom, yto, zfrom, zto, /* isMainStar = */ Boolean.TRUE, /* spectralClasses = */ null, new PageRequest(0, 10000));
            for (EddbBody mainStar : mainStars.getContent()) {
                if (StringUtils.isNotEmpty(mainStar.getSpectralClass())) {
                    Point p = this.coordToPoint(mainStar.getCoord());
                    float dy = Math.abs(mainStar.getCoord().getY() - coord.getY());
                    int alpha = 255 - Math.round((dy / (ysize / 2)) * 127);

                    Color color = StarUtil.spectralClassToColor(mainStar.getSpectralClass());
                    g.setColor(new Color(color.getRed(), color.getGreen(), color.getBlue(), alpha));
                    g.fillRect(p.x - 1, p.y - 1, 3, 3);
                }
            }

            for (int i = this.travelHistory.getVisitedSystems().size() - 1; i >= 0 && i >= this.travelHistory.getVisitedSystems().size() - 1000; i--) {
                VisitedSystem visitedSystem = this.travelHistory.getVisitedSystems().get(i);

                Point p = this.coordToPoint(visitedSystem.getCoord());
                float dy = Math.abs(visitedSystem.getCoord().getY() - coord.getY());
                if (dy <= (ysize / 2)) {
                    int alpha = 255 - Math.round((dy / (ysize / 2)) * 255);

                    g.setColor(new Color(80, 80, 80, alpha));
                    g.fillRect(p.x - 1, p.y - 1, 3, 3);

                    for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
                        if (scannedBody.getDistanceFromArrivalLS() != null && scannedBody.getDistanceFromArrivalLS().floatValue() == 0f) {
                            alpha = 255 - Math.round((dy / (ysize / 2)) * 127);

                            Color color = StarUtil.spectralClassToColor(scannedBody.getStarClass());
                            g.setColor(new Color(color.getRed(), color.getGreen(), color.getBlue(), alpha));
                            g.fillRect(p.x - 1, p.y - 1, 3, 3);
                            break;
                        }
                    }
                }
            }

            //            Page<EddbBody> blackHoles = eddbService.findStarsWithin(xfrom, xto, yfrom, yto, zfrom, zto, Boolean.TRUE, Arrays.asList("BH", "SMBH"), new PageRequest(0, 1000));
            //            for (EddbBody blackHole : blackHoles.getContent()) {
            //                Point p = this.coordToPoint(blackHole.getCoord());
            //
            //                g.setColor(new Color(80, 80, 80));
            //                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
            //                g.setColor(new Color(0, 0, 0));
            //                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
            //                g.setColor(this.travelHistory.isScanned(blackHole.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
            //                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
            //                g.drawString(blackHole.getName(), p.x + psize, p.y + psize / 2);
            //            }
            //
            //            Page<EddbBody> neutronStars = eddbService.findStarsWithin(xfrom, xto, yfrom, yto, zfrom, zto, Boolean.TRUE, Arrays.asList("NS"), new PageRequest(0, 1000));
            //            for (EddbBody neutronStar : neutronStars.getContent()) {
            //                Point p = this.coordToPoint(neutronStar.getCoord());
            //
            //                g.setColor(new Color(0, 0, 160));
            //                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
            //                g.setColor(new Color(255, 255, 255));
            //                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
            //                g.setColor(this.travelHistory.isScanned(neutronStar.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
            //                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
            //                g.drawString(neutronStar.getName(), p.x + psize, p.y + psize / 2);
            //            }
            //
            //            Page<EddbBody> earthLikeWorlds = eddbService.findPlanetsWithin(xfrom, xto, yfrom, yto, zfrom, zto, /* isTerraformingCandidate = */ null, Arrays.asList(EddbBody.TYPE_ID_EARTH_LIKE_WORLD), new PageRequest(0, 1000));
            //            for (EddbBody earthLikeWorld : earthLikeWorlds.getContent()) {
            //                Point p = this.coordToPoint(earthLikeWorld.getCoord());
            //
            //                g.setColor(new Color(0, 40, 120));
            //                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
            //                g.setColor(new Color(40, 180, 0));
            //                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
            //                g.setColor(this.travelHistory.isScanned(earthLikeWorld.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
            //                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
            //                g.drawString(earthLikeWorld.getName(), p.x + psize, p.y + psize / 2);
            //            }
            //
            //            Page<EddbBody> ammoniaWorlds = eddbService.findPlanetsWithin(xfrom, xto, yfrom, yto, zfrom, zto, /* isTerraformingCandidate = */ null, Arrays.asList(EddbBody.TYPE_ID_AMMONIA_WORLD), new PageRequest(0, 1000));
            //            for (EddbBody ammoniaWorld : ammoniaWorlds.getContent()) {
            //                Point p = this.coordToPoint(ammoniaWorld.getCoord());
            //
            //                g.setColor(new Color(140, 140, 40));
            //                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
            //                g.setColor(new Color(120, 120, 0));
            //                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
            //                g.setColor(this.travelHistory.isScanned(ammoniaWorld.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
            //                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
            //                g.drawString(ammoniaWorld.getName(), p.x + psize, p.y + psize / 2);
            //            }
            //
            //            Page<EddbBody> waterWorlds = eddbService.findPlanetsWithin(xfrom, xto, yfrom, yto, zfrom, zto, /* isTerraformingCandidate = */ null, Arrays.asList(EddbBody.TYPE_ID_WATER_WORLD), new PageRequest(0, 1000));
            //            for (EddbBody waterWorld : waterWorlds.getContent()) {
            //                Point p = this.coordToPoint(waterWorld.getCoord());
            //
            //                g.setColor(new Color(0, 0, 120));
            //                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
            //                g.setColor(new Color(0, 0, 80));
            //                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
            //                g.setColor(this.travelHistory.isScanned(waterWorld.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
            //                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
            //                g.drawString(waterWorld.getName(), p.x + psize, p.y + psize / 2);
            //            }
            //
            //            Page<EddbBody> terraformingCandidates = eddbService.findPlanetsWithin(xfrom, xto, yfrom, yto, zfrom, zto, /* isTerraformingCandidate = */ Boolean.TRUE, /* types = */ null, new PageRequest(0, 1000));
            //            for (EddbBody terraformingCandidate : terraformingCandidates.getContent()) {
            //                Point p = this.coordToPoint(terraformingCandidate.getCoord());
            //
            //                g.setColor(new Color(0, 160, 20));
            //                g.fillOval(p.x - poffset, p.y - poffset, psize, psize);
            //                g.setColor(new Color(200, 120, 50));
            //                g.fillOval((p.x - poffset) + 1, (p.y - poffset) + 1, psize - 2, psize - 2);
            //                g.setColor(this.travelHistory.isScanned(terraformingCandidate.getName()) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
            //                g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
            //                g.drawString(terraformingCandidate.getName(), p.x + psize, p.y + psize / 2);
            //            }

            // Me
            g.setColor(Color.RED); // Unknown on EDDB
            try {
                EddbSystem system = eddbService.findSystemByName(this.travelHistory.getSystemName());
                if (system != null) {
                    g.setColor(Color.ORANGE); // Coords known on EDDB
                    List<EddbBody> bodies = eddbService.findBodiesOfSystem(system.getId());
                    for (EddbBody body : bodies) {
                        if (Boolean.TRUE.equals(body.getIsMainStar())) {
                            if (StringUtils.isNotEmpty(body.getStarClass())) {
                                g.setColor(Color.GREEN); // Main star spectral class known on EDDB
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

            g.setColor(Color.CYAN);
            g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
            for (String commanderName : this.commanderLocations.keySet()) {
                p = this.coordToPoint(this.commanderLocations.get(commanderName));
                g.fillRect(p.x - poffset, p.y - poffset, psize, psize);
                g.drawString(commanderName, p.x, p.y);
            }
        }

        private Point coordToPoint(Coord coord) {
            float xPercent = (coord.getX() - this.xfrom) / this.xsize;
            float yPercent = 1.0f - ((coord.getZ() - this.zfrom) / this.zsize);

            return new Point(Math.round(xPercent * this.getWidth()), Math.round(yPercent * this.getHeight()));
        }

    }

}
