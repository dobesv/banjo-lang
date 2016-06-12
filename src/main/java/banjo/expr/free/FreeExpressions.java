package banjo.expr.free;

import banjo.eval.resolver.GlobalRef;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.Resolver;
import banjo.expr.free.FreeSingletonKernelObject.SingletonKernelObjectFactory;
import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;

public class FreeExpressions {

    public static final FreeExpression SLOT_MAPPER_FACTORY = FreeExpression.kernelObject(Identifier.SLOT_MAPPER.id, new SingletonKernelObjectFactory() {

        @Override
        public <T> T apply(Resolver<T> resolver, InstanceAlgebra<T> algebra) {
            return algebra.slotMapperFactory();
        }
    });
    public static final FreeExpression ARG_MAPPER_FACTORY = FreeExpression.kernelObject(Identifier.ARG_MAPPER.id, new SingletonKernelObjectFactory() {

        @Override
        public <T> T apply(Resolver<T> resolver, InstanceAlgebra<T> algebra) {
            return algebra.argMapperFactory();
        }
    });
    public static final FreeExpression DYNAMIC_CALL_PROXY_FACTORY =
        FreeExpression.kernelObject(Identifier.DYNAMIC_CALL_PROXY.id, new SingletonKernelObjectFactory() {

            @Override
            public <T> T apply(Resolver<T> resolver, InstanceAlgebra<T> algebra) {
                return algebra.dynamicCallProxyFactory();
            }
        });
    public static final FreeExpression DYNAMIC_SLOT_PROXY_FACTORY =
        FreeExpression.kernelObject(Identifier.DYNAMIC_SLOT_PROXY.id, new SingletonKernelObjectFactory() {

            @Override
            public <T> T apply(Resolver<T> resolver, InstanceAlgebra<T> algebra) {
                return algebra.dynamicSlotProxyFactory();
            }
        });
    public static final FreeExpression EXTEND_FUNCTION = FreeExpression.kernelObject(Operator.EXTENSION.getOp(), new SingletonKernelObjectFactory() {

        @Override
        public <T> T apply(Resolver<T> resolver, InstanceAlgebra<T> algebra) {
            return algebra.extendFunction();
        }
    });
    public static final FreeExpression FAIL_FUNCTION = FreeExpression.kernelObject(Identifier.FAIL.id, new SingletonKernelObjectFactory() {

        @Override
        public <T> T apply(Resolver<T> resolver, InstanceAlgebra<T> algebra) {
            return algebra.failFunction();
        }
    });

    public static final FreeExpression FUNCTION_COMPOSITION_FUNCTION = FreeExpression.kernelObject(Identifier.COMPOSE.id, new SingletonKernelObjectFactory() {
        @Override
        public <T> T apply(banjo.eval.resolver.Resolver<T> resolver, banjo.eval.resolver.InstanceAlgebra<T> algebra) {
            return algebra.functionCompositionFunction();
        }
    });

    public static final FreeExpression TRUE_VALUE = FreeExpression.global(GlobalRef.TRUE);
    public static final FreeExpression FUNCTION_TRAIT = FreeExpression.global(GlobalRef.FUNCTION_TRAIT);

    public static final FreeExpression TYPE_UNION_FACTORY = FreeExpression.kernelObject(Identifier.TYPE_UNION.id, new SingletonKernelObjectFactory() {

        @Override
        public <T> T apply(Resolver<T> resolver, InstanceAlgebra<T> algebra) {
            return algebra.slotMapperFactory();
        }
    });

    public static final FreeExpression KERNEL_TRUE = FreeExpression.kernelBoolean(true);
    public static final FreeExpression KERNEL_FALSE = FreeExpression.kernelBoolean(false);

    public static final FreeExpression EMPTY_LIST = FreeExpression.global(GlobalRef.EMPTY_LIST);

    public static final FreeExpression SINGLE_ELEMENT_LIST_FACTORY = FreeExpression.global(GlobalRef.SINGLE_ELEMENT_LIST_FACTORY);

}
