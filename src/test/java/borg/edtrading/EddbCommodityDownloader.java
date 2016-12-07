package borg.edtrading;

import borg.edtrading.data.Blueprint;
import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.eddb.data.EddbCommodity;
import borg.edtrading.eddb.repositories.EddbCommodityRepository;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

/**
 * EddbCommodityDownloader
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbCommodityDownloader {

    static final Logger logger = LogManager.getLogger(EddbCommodityDownloader.class);

    private static final AnnotationConfigApplicationContext APPCTX = new AnnotationConfigApplicationContext(Config.class);

    public static void main(String[] args) throws Exception {
        EddbCommodityRepository commodityRepo = APPCTX.getBean(EddbCommodityRepository.class);
        Page<EddbCommodity> page = commodityRepo.findAll(new PageRequest(0, 9999));
        List<EddbCommodity> commodities = page.getContent().stream().filter(c -> c.getName() != null && c.getCategory() != null).sorted((c1, c2) -> c1.getName().toLowerCase().compareTo(c2.getName().toLowerCase()))
                .sorted((c1, c2) -> c1.getCategory().getName().toLowerCase().compareTo(c2.getCategory().getName().toLowerCase())).collect(Collectors.toList());
        commodities.forEach(c -> {
            String enumConstant = c.getName().toUpperCase().replace(" ", "_").replaceAll("\\W", "");
            String name = "\"" + c.getName() + "\"";
            String journalName = "\"" + c.getName().toLowerCase().replaceAll("\\W", "") + "\"";
            String typeEnum = "ItemType.COMMODITY";
            String categoryName = "\"" + c.getCategory().getName() + "\"";
            int grade = 0;
            if (Boolean.TRUE.equals(c.getIsRare())) {
                grade = 2;
            } else if (usedInBlueprints(c.getName())) {
                grade = 1;
            }
            System.out.println(String.format(Locale.US, "    %-50s(%-50s, %-50s, %-25s, %-25s, %d),", enumConstant, name, journalName, typeEnum, categoryName, grade));
        });
    }

    private static boolean usedInBlueprints(String commodityName) {
        Item item = Item.byName(commodityName);
        if (item == null) {
            item = Item.byJournalName(commodityName);
        }
        if (item == null) {
            item = Item.findBestMatching(commodityName, ItemType.COMMODITY);
        }
        if (item != null) {
            for (Blueprint blueprint : Blueprint.values()) {
                if (blueprint.getIngredients().containsKey(item)) {
                    return true;
                }
            }
        }
        return false;
    }

}
