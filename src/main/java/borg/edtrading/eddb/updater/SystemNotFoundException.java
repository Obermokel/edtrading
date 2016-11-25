package borg.edtrading.eddb.updater;

/**
 * SystemNotFoundException
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SystemNotFoundException extends Exception {

    private static final long serialVersionUID = 7765555948682577982L;

    public SystemNotFoundException() {
        super();
    }

    public SystemNotFoundException(String message) {
        super(message);
    }

    public SystemNotFoundException(Throwable cause) {
        super(cause);
    }

    public SystemNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }

}
