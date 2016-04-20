package banjo.expr.typed;

import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.Slot;
import fj.Ord;
import fj.P;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

/**
 * The type of an object's slots are not fully known until all extension
 * operations are accounted for. Therefore when we encounter an object literal
 * during the typing operation we just have to capture the environment and
 * continue.
 */
public class ObjectLiteralType implements ExprType {
    public static final ExprType EMPTY = null;
    public final TypeEnvironment env;
    public final TreeMap<String, Slot> slotMap;

    public ObjectLiteralType(TypeEnvironment env, ObjectLiteral objectLiteral) {
        this.env = env;
        this.slotMap = TreeMap.treeMap(Ord.stringOrd, objectLiteral.slots.map(s -> P.p(s.name.id, s)));
    }

    public ObjectLiteralType(TypeEnvironment env, TreeMap<String, Slot> slotMap) {
        super();
        this.env = env;
        this.slotMap = slotMap;
    }

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return visitor.objectLiteral(this);
    }

    @Override
    public ExprType slot(List<ExprType> trace, String slotName, ExprType selfType, ExprType fallbackType) {
        Option<Slot> maybeSlot = slotMap.get(slotName);
        if(maybeSlot.isNone()) {
            return ExprType.super.slot(trace, slotName, selfType, fallbackType);
        }
        Slot slot = maybeSlot.some();
        TypeEnvironment slotEnv = slot.sourceObjectBinding.map(b -> env.bind(
            b.id,
            fallbackType == null ? TypeBinding.slot(selfType, slotName) : TypeBinding.slotWithBase(selfType, slotName, fallbackType))).orSome(env);
        return ExprTypeCalculator.typeInEnv(trace, slotEnv, slot.value);
    }

}
