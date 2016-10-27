package borg.edtrading.ocr.fixer;

/**
 * FixerException
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FixerException extends Exception {

    private static final long serialVersionUID = 2613005474629697720L;

    public FixerException() {
        super();
    }

    public FixerException(String message) {
        super(message);
    }

    public FixerException(Throwable cause) {
        super(cause);
    }

    public FixerException(String message, Throwable cause) {
        super(message, cause);
    }

}
