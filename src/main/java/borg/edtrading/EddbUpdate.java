package borg.edtrading;

import borg.edtrading.cfg.Config;
import borg.edtrading.eddn.EddnReaderThread;
import borg.edtrading.services.EddbService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.io.IOException;

/**
 * EddbUpdate
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbUpdate {

    static final Logger logger = LogManager.getLogger(EddbUpdate.class);

    private static final AnnotationConfigApplicationContext APPCTX = new AnnotationConfigApplicationContext(Config.class);

    public static void main(String[] args) throws IOException {
        new EddnReaderThread();

        APPCTX.getBean(EddbService.class).updateEddbData(/* forceReindex = */ true);
    }

}
