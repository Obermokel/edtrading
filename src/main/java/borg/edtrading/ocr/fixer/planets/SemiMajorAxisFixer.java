package borg.edtrading.ocr.fixer.planets;

import borg.edtrading.ocr.fixer.AbstractBigDecimalWithOptionalUnitFixer;

public class SemiMajorAxisFixer extends AbstractBigDecimalWithOptionalUnitFixer {

    @Override
    protected String getUnit() {
        return "AU";
    }

    @Override
    protected boolean patternHasSign() {
        return false;
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
