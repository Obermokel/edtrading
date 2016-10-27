package borg.edtrading.ocr.fixer.planets;

import borg.edtrading.ocr.fixer.AbstractBigDecimalWithOptionalUnitFixer;

public class AxialTiltFixer extends AbstractBigDecimalWithOptionalUnitFixer {

    @Override
    protected String getUnit() {
        return "°";
    }

    @Override
    protected boolean patternHasSign() {
        return true;
    }

    @Override
    protected boolean patternHasThousands() {
        return false;
    }

    @Override
    protected int patternNumDecimals() {
        return 2;
    }

}
