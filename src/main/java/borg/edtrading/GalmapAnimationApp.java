package borg.edtrading;

import borg.edtrading.cfg.Config;
import borg.edtrading.cfg.Constants;
import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.gui.MapCreator;
import borg.edtrading.gui.MapCreator.MapView;
import borg.edtrading.services.EddbService;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.SortBuilder;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.SearchQuery;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.imageio.ImageIO;

/**
 * GalmapApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GalmapAnimationApp {

    static final Logger logger = LogManager.getLogger(GalmapAnimationApp.class);

    private static final AnnotationConfigApplicationContext APPCTX = new AnnotationConfigApplicationContext(Config.class);

    private static final SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");

    public static void main(String[] args) throws Exception {
        APPCTX.getBean(EddbService.class).updateEddbData(false);

        Date release_2_0 = df.parse("2015-12-15");
        Date release_2_1 = df.parse("2016-05-26");
        Date release_2_2 = df.parse("2016-10-25");

        Date today = DateUtils.truncate(new Date(), Calendar.DATE);
        //Date date = df.parse("2015-05-13"); // Where EDSM starts
        //Date date = df.parse("2016-09-18"); // Where EDDB starts
        // Patch 2.0 Horizons:          2015-12-15
        // Barnacles:                   2016-01-15
        // Distant worlds:              2016-01-14 .. 2016-06-05
        // Patch 2.1 Engineers:         2016-05-26
        // Crab Nebula Expedition:      2016-06-11 .. 2016-07-18
        // Jaques discovered:           2016-06-29
        // August Exodus:               2016-07-24 .. 2016-10-04
        // ???
        // Patch 2.2 neutron jumps:     2016-10-25
        // Children of Raxxla:          2016-11-17 .. 2016-12-01
        // CEI & CCC:                   2016-Dec
        // Thargoid hyperdictions:      2017-01-06
        // Distant stars:               2017-01-15 .. 2017-01-29
        // Ancient ruins puzzle:        2017-01-10 .. 2017-Feb
        Date date = df.parse("2017-02-13");
        ElasticsearchTemplate elasticsearchTemplate = APPCTX.getBean(ElasticsearchTemplate.class);
        Map<Long, EddbBody> mainStarsBySystem = findMainStarsBySystem(elasticsearchTemplate);

        File statsCsvFile = new File(Constants.TEMP_DIR, "stats.csv");
        FileUtils.write(statsCsvFile, "Datum;Systeme;Hauptsterne\r\n", "ISO-8859-1", false);
        while (date.before(today)) {
            System.out.print(df.format(date) + ": ");
            int nSystems = 0;
            int nMainStars = 0;
            int nNeutronStars = 0;

            MapCreator mapCreatorGalaxy = new MapCreator(-43000, 41000, -3333, 3333, -18000, 66000, MapView.TOP, 4096);
            mapCreatorGalaxy.prepare();
            MapCreator mapCreatorSol = new MapCreator(-500, 500, -3333, 3333, -500, 500, MapView.TOP, 1200);
            mapCreatorSol.prepare();

            BoolQueryBuilder qb = QueryBuilders.boolQuery();
            qb.must(QueryBuilders.rangeQuery("createdAt").lte(date.getTime()));
            qb.must(QueryBuilders.rangeQuery("coord.y").from(-3333.0).to(3333.0));
            SortBuilder sb = SortBuilders.fieldSort("coord.y").order(SortOrder.ASC);
            SearchQuery searchQuery = new NativeSearchQueryBuilder().withIndices("eddbsystem").withQuery(qb).withSort(sb).withPageable(new PageRequest(0, 1000)).build();
            String scrollId = elasticsearchTemplate.scan(searchQuery, 1000, false);
            boolean hasRecords = true;
            while (hasRecords) {
                Page<EddbSystem> page = elasticsearchTemplate.scroll(scrollId, 5000, EddbSystem.class);
                if (page.hasContent()) {
                    System.out.print(".");
                    for (EddbSystem system : page.getContent()) {
                        String starClass = null;
                        try {
                            EddbBody mainStar = mainStarsBySystem.get(system.getId());
                            if (mainStar != null && mainStar.getCreatedAt() != null && mainStar.getCreatedAt().compareTo(date) <= 0) {
                                starClass = mainStar.getStarClass();
                            }
                        } catch (Exception e) {
                            // Ignore
                        }

                        if (system.getCreatedAt().before(release_2_0)) {
                            mapCreatorGalaxy.drawStar(system.getCoord(), starClass, null, 7, 128, 127);
                            mapCreatorSol.drawStar(system.getCoord(), starClass, null, 7, 128, 127);
                        } else if (system.getCreatedAt().before(release_2_1)) {
                            mapCreatorGalaxy.drawStar(system.getCoord(), starClass, null, 5, 128, 127);
                            mapCreatorSol.drawStar(system.getCoord(), starClass, null, 5, 128, 127);
                        } else if (system.getCreatedAt().before(release_2_2)) {
                            mapCreatorGalaxy.drawStar(system.getCoord(), starClass, null, 3, 128, 127);
                            mapCreatorSol.drawStar(system.getCoord(), starClass, null, 3, 128, 127);
                        } else {
                            if (StringUtils.isEmpty(starClass)) {
                                mapCreatorGalaxy.drawStar(system.getCoord(), starClass, null, 1, 128, 127);
                                mapCreatorSol.drawStar(system.getCoord(), starClass, null, 1, 128, 127);
                            } else {
                                mapCreatorGalaxy.drawStar(system.getCoord(), starClass, null, 3, 128, 127);
                                mapCreatorSol.drawStar(system.getCoord(), starClass, null, 3, 128, 127);
                            }
                        }

                        nSystems++;
                        if (StringUtils.isNotEmpty(starClass)) {
                            nMainStars++;
                            if ("N".equals(starClass) || "NS".equals(starClass)) {
                                nNeutronStars++;
                            }
                        }
                    }
                } else {
                    hasRecords = false;
                }
            }
            elasticsearchTemplate.clearScroll(scrollId);

            // Draw reference texts
            mapCreatorGalaxy.drawString(new Coord(0.0f, 0.0f, 0.0f), "Sol", Color.WHITE, new Font("Sans Serif", Font.BOLD, 64));
            mapCreatorGalaxy.drawString(new Coord(25.21875f, -20.90625f, 25899.96875f), "Sagittarius A*", Color.WHITE, new Font("Sans Serif", Font.BOLD, 64));
            mapCreatorGalaxy.drawString(new Coord(-1111.5625f, -134.21875f, 65269.75f), "Beagle Point", Color.WHITE, new Font("Sans Serif", Font.BOLD, 64));
            mapCreatorGalaxy.drawString(new Coord(-9530.5f, -910.28125f, 19808.125f), "Colonia", Color.WHITE, new Font("Sans Serif", Font.BOLD, 48));
            mapCreatorGalaxy.drawString(new Coord(-5802.0f, 123.0f, -5968.0f), "Formidine Rift", Color.WHITE, new Font("Sans Serif", Font.BOLD, 48));
            mapCreatorGalaxy.drawString(new Coord(-3191.0f, 71.0f, 8593.0f), "The Conflux", Color.WHITE, new Font("Sans Serif", Font.BOLD, 48));
            mapCreatorGalaxy.drawString(new Coord(7889.0f, 134.0f, 7507.0f), "Hawkin's Gap", Color.WHITE, new Font("Sans Serif", Font.BOLD, 48));
            mapCreatorGalaxy.drawString(new Coord(558.5f, -707.40625f, -6941.75f), "Crab Nebula", Color.WHITE, new Font("Sans Serif", Font.BOLD, 32));
            mapCreatorGalaxy.drawString(new Coord(-10467.1875f, -878.59375f, -5035.78125f), "Rho Cassiopeiae", Color.WHITE, new Font("Sans Serif", Font.BOLD, 32));

            mapCreatorSol.drawString(new Coord(0.0f, 0.0f, 0.0f), "Sol", Color.WHITE, new Font("Sans Serif", Font.BOLD, 32));
            mapCreatorSol.drawString(new Coord(-81.78125f, -149.4375f, -343.375f), "Maia", Color.WHITE, new Font("Sans Serif", Font.BOLD, 24));
            mapCreatorSol.drawString(new Coord(-303.40625f, 7.3125f, -314.15625f), "Robigo", Color.WHITE, new Font("Sans Serif", Font.BOLD, 24));
            //mapCreatorSol.drawString(new Coord(-348.65625f, 13.875f, -339.21875f), "Ceos/Sothis", Color.WHITE, new Font("Sans Serif", Font.BOLD, 24));
            mapCreatorSol.drawString(new Coord(46.375f, -448.6875f, -127.125f), "Fehu", Color.WHITE, new Font("Sans Serif", Font.BOLD, 24));
            mapCreatorSol.drawString(new Coord(-305.75f, 272.21875f, 49.5f), "17 Draconis", Color.WHITE, new Font("Sans Serif", Font.BOLD, 24));
            mapCreatorSol.drawString(new Coord(357.34375f, -49.34375f, -74.75f), "Ancient Ruins", Color.WHITE, new Font("Sans Serif", Font.BOLD, 24));
            mapCreatorSol.drawString(new Coord(169.40625f, -72.5f, -462.625f), "Betelgeuse", Color.WHITE, new Font("Sans Serif", Font.BOLD, 32));
            mapCreatorSol.drawString(new Coord(1576.0625f, -150.21875f, -922.53125f), "VY Canis", Color.WHITE, new Font("Sans Serif", Font.BOLD, 32));
            mapCreatorSol.drawString(new Coord(1576.0625f, -150.21875f, -1042.53125f), "Majoris", Color.WHITE, new Font("Sans Serif", Font.BOLD, 32));
            mapCreatorSol.drawString(new Coord(700.0f, 0f, -2000.0f), "Orion", Color.WHITE, new Font("Sans Serif", Font.BOLD, 32));
            mapCreatorSol.drawString(new Coord(700.0f, 0f, -2120.0f), "Nebula", Color.WHITE, new Font("Sans Serif", Font.BOLD, 32));
            mapCreatorSol.drawString(new Coord(1525f, 1728.3125f, -2000f), "M67", Color.WHITE, new Font("Sans Serif", Font.BOLD, 32));

            BufferedImage mapImage = mapCreatorGalaxy.finish();
            Graphics2D g = mapImage.createGraphics();
            g.setColor(Color.WHITE);
            g.setFont(new Font("Consolas", Font.BOLD, 256));
            g.drawString(df.format(date), 50, 250);
            g.setFont(new Font("Consolas", Font.BOLD, 64));
            g.drawString(String.format(Locale.US, "Systems:       %,10d", nSystems), 3200, 3925);
            g.drawString(String.format(Locale.US, "Main stars:    %,10d", nMainStars), 3200, 4000);
            g.drawString(String.format(Locale.US, "Neutron stars: %,10d", nNeutronStars), 3200, 4075);
            g.dispose();
            ImageIO.write(mapImage, "png", new File(Constants.TEMP_DIR, "Map " + df.format(date) + ".png"));

            mapImage = mapCreatorSol.finish();
            ImageIO.write(mapImage, "png", new File(Constants.TEMP_DIR, "Sol " + df.format(date) + ".png"));

            date = DateUtils.addDays(date, 1);

            System.out.println();
            FileUtils.write(statsCsvFile, new SimpleDateFormat("dd.MM.yyyy").format(date) + ";" + nSystems + ";" + nMainStars + "\r\n", "ISO-8859-1", true);
        }
    }

    private static Map<Long, EddbBody> findMainStarsBySystem(ElasticsearchTemplate elasticsearchTemplate) {
        Map<Long, EddbBody> result = new HashMap<>();

        BoolQueryBuilder qb = QueryBuilders.boolQuery();
        qb.must(QueryBuilders.termQuery("isMainStar", true));
        qb.must(QueryBuilders.existsQuery("starClass"));
        SearchQuery searchQuery = new NativeSearchQueryBuilder().withIndices("eddbbody").withQuery(qb).withPageable(new PageRequest(0, 1000)).build();
        String scrollId = elasticsearchTemplate.scan(searchQuery, 1000, false);
        boolean hasRecords = true;
        while (hasRecords) {
            Page<EddbBody> page = elasticsearchTemplate.scroll(scrollId, 5000, EddbBody.class);
            if (page.hasContent()) {
                for (EddbBody body : page.getContent()) {
                    result.put(body.getSystemId(), body);
                }
            } else {
                hasRecords = false;
            }
        }
        elasticsearchTemplate.clearScroll(scrollId);

        return result;
    }

}
