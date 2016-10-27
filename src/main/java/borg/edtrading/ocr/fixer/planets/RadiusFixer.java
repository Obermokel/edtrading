package borg.edtrading.ocr.fixer.planets;

import borg.edtrading.ocr.fixer.AbstractBigDecimalWithOptionalUnitFixer;

public class RadiusFixer extends AbstractBigDecimalWithOptionalUnitFixer {

    @Override
    protected String getUnit() {
        return "KM";
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
        return 0;
    }

}
