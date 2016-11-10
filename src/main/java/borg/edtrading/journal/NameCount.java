package borg.edtrading.journal;

import java.io.Serializable;

/**
 * NameCount
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class NameCount implements Serializable {

    private static final long serialVersionUID = -9182252618244153576L;

    private final String name;
    private final Integer count;

    public NameCount(String name, Integer count) {
        this.name = name;
        this.count = count;
    }

    public String getName() {
        return this.name;
    }

    public Integer getCount() {
        return this.count;
    }

}
