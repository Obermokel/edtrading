package borg.edtrading.data;

/**
 * Component
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public enum Component {

    //@formatter:off
    FSD("Frameshift Drive");
    //@formatter:on

    private final String name;

    private Component(String name) {
        this.name = name;
    }

    public String getName() {
        return this.name;
    }

}
