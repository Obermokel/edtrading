package borg.edtrading.data;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Commodity
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Commodity {

    static final Logger logger = LogManager.getLogger(Commodity.class);

    private String name = null;
    private Long galacticAverage = null;

    public Commodity(String name, Long galacticAverage) {
        this.setName(name);
        this.setGalacticAverage(galacticAverage);
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Long getGalacticAverage() {
        return this.galacticAverage;
    }

    public void setGalacticAverage(Long galacticAverage) {
        this.galacticAverage = galacticAverage;
    }

}
