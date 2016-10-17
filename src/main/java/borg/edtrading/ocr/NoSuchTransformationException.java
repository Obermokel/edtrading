package borg.edtrading.ocr;

/**
 * NoSuchTransformationException
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class NoSuchTransformationException extends RuntimeException {

    private static final long serialVersionUID = 1987894095878514701L;

    public NoSuchTransformationException() {
        super();
    }

    public NoSuchTransformationException(String message) {
        super(message);
    }

    public NoSuchTransformationException(Throwable cause) {
        super(cause);
    }

    public NoSuchTransformationException(String message, Throwable cause) {
        super(message, cause);
    }

}
