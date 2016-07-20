package borg.edtrading.data;

import java.util.Map;

/**
 * Blueprint
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public enum Blueprint {

    //@formatter:off
    FSD___INCREASED_RANGE___1(Component.FSD, Modification.FSD___INCREASED_RANGE, 1, new BlueprintBuilder()
            .add(Item.ANTIMONY, 2)
            .build());
    //@formatter:on

    private final Component component;
    private final Modification modification;
    private final int grade;
    private final Map<Item, Integer> items;

    private Blueprint(Component component, Modification modification, int grade, Map<Item, Integer> items) {
        this.component = component;
        this.modification = modification;
        this.grade = grade;
        this.items = items;
    }

    public Component getComponent() {
        return this.component;
    }

    public Modification getModification() {
        return this.modification;
    }

    public int getGrade() {
        return this.grade;
    }

    public Map<Item, Integer> getItems() {
        return this.items;
    }

}
