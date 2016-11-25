package borg.edtrading.eddb.data;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * EddbData
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbData implements Serializable {

    private static final long serialVersionUID = 7541365923395699416L;

    static final Logger logger = LogManager.getLogger(EddbData.class);

    private final Map<Long, EddbSystem> systemsById;
    private final Map<String, EddbSystem> systemsByName;

    public EddbData(Map<Long, EddbSystem> systemsById) {
        this.systemsById = systemsById;
        this.systemsByName = systemsById.values().parallelStream().collect(Collectors.toMap(EddbSystem::getName, Function.identity()));
        logger.debug("Mapped " + systemsById.size() + " systems by " + this.systemsByName.size() + " names");
    }

}
