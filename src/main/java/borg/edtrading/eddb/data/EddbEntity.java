package borg.edtrading.eddb.data;

import java.io.Serializable;
import java.util.Date;

/**
 * EddbEntity
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbEntity extends Serializable {

    Long getId();

    Date getUpdatedAt();

}
