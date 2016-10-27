package borg.edtrading.ocr.fixer.planets;

import borg.edtrading.ocr.fixer.AbstractBigDecimalWithOptionalUnitFixer;

public class SurfacePressureFixer extends AbstractBigDecimalWithOptionalUnitFixer {

    @Override
    protected String getUnit() {
        return "ATMOSPHERES";
    }

    @Override
    protected boolean patternHasSign() {
        return false;
    }

    @Override
    protected boolean patternHasThousands() {
        return true;
    }

    @Override
    protected int patternNumDecimals() {
        return 2;
    }

}
