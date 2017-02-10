package borg.edtrading;

import borg.edtrading.cfg.Config;
import borg.edtrading.cfg.Constants;
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

        Date today = DateUtils.truncate(new Date(), Calendar.DATE);
        Date date = df.parse("2015-05-13"); // Where EDSM starts
        //Date date = df.parse("2016-09-18"); // Where EDDB starts
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
        //Date date = df.parse("2017-02-08");
        ElasticsearchTemplate elasticsearchTemplate = APPCTX.getBean(ElasticsearchTemplate.class);
        Map<Long, EddbBody> mainStarsBySystem = findMainStarsBySystem(elasticsearchTemplate);

        File statsCsvFile = new File(Constants.TEMP_DIR, "stats.csv");
        FileUtils.write(statsCsvFile, "Datum;Systeme;Hauptsterne\r\n", "ISO-8859-1", false);
        while (date.before(today)) {
            System.out.print(df.format(date) + ": ");
            int nSystems = 0;
            int nMainStars = 0;

            MapCreator mapCreator = new MapCreator(-43000, 41000, -3333, 3333, -18000, 66000, MapView.TOP, 4096);
            mapCreator.prepare();

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

                        mapCreator.drawStar(system.getCoord(), starClass, null);

                        nSystems++;
                        if (StringUtils.isNotEmpty(starClass)) {
                            nMainStars++;
                        }
                    }
                } else {
                    hasRecords = false;
                }
            }
            elasticsearchTemplate.clearScroll(scrollId);

            BufferedImage mapImage = mapCreator.finish();
            Graphics2D g = mapImage.createGraphics();
            g.setFont(new Font("Consolas", Font.BOLD, 256));
            g.setColor(Color.WHITE);
            g.drawString(df.format(date), 50, 250);
            g.dispose();
            ImageIO.write(mapImage, "png", new File(Constants.TEMP_DIR, "Map " + df.format(date) + ".png"));

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
