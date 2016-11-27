package borg.edtrading;

import borg.edtrading.data.Blueprint;
import borg.edtrading.data.Component;
import borg.edtrading.data.Engineer;
import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalReader;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * EngineerSpecialEffectOptimizer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EngineerSpecialEffectOptimizer {

    static final Logger logger = LogManager.getLogger(EngineerSpecialEffectOptimizer.class);

    public static void main(String[] args) throws Exception {
        // This is what we want to have, and how many.
        // Assumes the engineer is currently unlocked at lvl 5.
        LinkedHashMap<Blueprint, Integer> wantHave = new LinkedHashMap<>();
        wantHave.put(Blueprint.MULTICANNON___OVERCHARGED_WEAPON___GRADE_5, 1);
        wantHave.put(Blueprint.CANNON___STURDY_MOUNT___GRADE_5, 1);
        wantHave.put(Blueprint.PULSE_LASER___RAPID_FIRE_MODIFICATION___GRADE_5, 4);
        wantHave.put(Blueprint.PLASMA_ACCELERATOR___OVERCHARGED_WEAPON___GRADE_5, 1);

        // How many attempts to make at grade 5.
        int numGrade5Rolls = 3;

        // The list of available components allows to efficiently choose blueprints for leveling
        // up to level 5 again. Of course we should have the components for which we want the special
        // effect, we also always have the core modules, but maybe also some other ones. Most
        // ships for example have a shield generator.
        Set<Component> availableComponents = wantHave.keySet().stream().map(Blueprint::getComponent).collect(Collectors.toSet());
        availableComponents.add(Component.ARMOUR);
        availableComponents.add(Component.POWER_PLANT);
        availableComponents.add(Component.THRUSTERS);
        availableComponents.add(Component.FRAME_SHIFT_DRIVE);
        availableComponents.add(Component.POWER_DISTRIBUTOR);
        availableComponents.add(Component.LIFE_SUPPORT);

        Journal journal = new Journal(new JournalReader().readEntireJournal(Constants.JOURNAL_DIR));
        Inventory inventory = new Inventory("Mokel DeLorean [GPL]", journal);

        LinkedHashMap<Item, Integer> totalCost = new LinkedHashMap<>();
        for (Blueprint blueprint : wantHave.keySet()) {
            Engineer engineer = blueprint.getEngineers().get(0);
            List<Blueprint> grade3Blueprints = findBlueprints(engineer, 3, availableComponents);
            List<Blueprint> grade4Blueprints = findBlueprints(engineer, 4, availableComponents);

            int n = wantHave.get(blueprint);
            for (int i = 0; i < n; i++) {
                for (int a = 0; a < numGrade5Rolls; a++) {
                    logger.debug("Crafting " + blueprint + " @ " + engineer);
                    doCraft(blueprint, inventory, totalCost);
                }
                for (int l = 0; l < 3; l++) {
                    Blueprint levelUpBlueprint = chooseBestLevelUpBlueprint(grade3Blueprints, inventory);
                    logger.debug("Level up: " + levelUpBlueprint);
                    doCraft(levelUpBlueprint, inventory, totalCost);
                }
                for (int l = 0; l < 3; l++) {
                    Blueprint levelUpBlueprint = chooseBestLevelUpBlueprint(grade4Blueprints, inventory);
                    logger.debug("Level up: " + levelUpBlueprint);
                    doCraft(levelUpBlueprint, inventory, totalCost);
                }
                LinkedHashMap<Item, Integer> haveNotEnough = new LinkedHashMap<>();
                for (Item item : Item.values()) {
                    if (item.getType() != ItemType.COMMODITY && inventory.getHave(item.getName()) < 0) {
                        haveNotEnough.put(item, inventory.getHave(item.getName()));
                    }
                }
                MiscUtil.sortMapByValue(haveNotEnough);
                MiscUtil.sortMapByValueReverse(totalCost);
                logger.debug("Total cost:       " + totalCost);
                logger.debug("Have not enough:  " + haveNotEnough);
            }
        }
    }

    private static Blueprint chooseBestLevelUpBlueprint(List<Blueprint> blueprints, Inventory inventory) {
        LinkedHashMap<Blueprint, Float> matsPrioByBlueprint = new LinkedHashMap<>();
        for (Blueprint bp : blueprints) {
            float totalPrio = 0f;
            for (Item item : bp.getIngredients().keySet()) {
                if (item.getType() != ItemType.COMMODITY) {
                    totalPrio += inventory.getPriority(item.getName());
                }
            }
            matsPrioByBlueprint.put(bp, totalPrio);
        }
        MiscUtil.sortMapByValue(matsPrioByBlueprint);
        return matsPrioByBlueprint.keySet().iterator().next();
    }

    private static List<Blueprint> findBlueprints(Engineer engineer, int grade, Set<Component> availableComponents) {
        List<Blueprint> result = new ArrayList<>();
        for (Blueprint bp : Blueprint.values()) {
            if (bp.getEngineers().contains(engineer) && bp.getGrade() == grade && availableComponents.contains(bp.getComponent())) {
                result.add(bp);
            }
        }
        return result;
    }

    private static void doCraft(Blueprint blueprint, Inventory inventory, Map<Item, Integer> totalCost) {
        Map<Item, Integer> ingredients = blueprint.getIngredients();
        for (Item item : ingredients.keySet()) {
            inventory.spent(item.getName(), ingredients.get(item), item.getType());
            totalCost.put(item, totalCost.getOrDefault(item, 0) + ingredients.get(item));
        }
    }

    private static int canCraft(Blueprint blueprint, Inventory inventory) {
        return canCraft(blueprint, inventory, true);
    }

    private static int canCraft(Blueprint blueprint, Inventory inventory, boolean ignoreCommodities) {
        int result = 999999999;
        for (Item item : blueprint.getIngredients().keySet()) {
            if (ignoreCommodities && item.getType() == ItemType.COMMODITY) {
                // Ignore
            } else {
                int have = inventory.getHave(item.getName());
                int cost = blueprint.getIngredients().get(item);
                result = Math.min(result, have / cost);
            }
        }
        return Math.max(0, result);
    }

}
