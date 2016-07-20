package borg.edtrading.data;

/**
 * Modification
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public enum Modification {

    //@formatter:off
    FSD___INCREASED_RANGE("FSD - Increased Range");
    //@formatter:on

    private final String name;

    private Modification(String name) {
        this.name = name;
    }

    public String getName() {
        return this.name;
    }

}
