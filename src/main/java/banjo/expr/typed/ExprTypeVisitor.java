package banjo.expr.typed;

public interface ExprTypeVisitor<T> {

    T functionLiteral(FunctionLiteralType functionLiteralType);

    T objectLiteral(ObjectLiteralType objectLiteralType);

    T kernelNumber(KernelNumberType kernelNumberType);

    T kernelString(KernelStringType kernelStringType);

    T unknown(UnknownType unknownType);

    T extendedObject(ExtendedObjectType extendType);

    T kernelFunction(KernelFunctionType kernelFunctionType);

    T kernelBiFunction(KernelBiFunctionType kernelBiFunctionType);

    T kernelBoolean(KernelBooleanType kernelBooleanType);

}
