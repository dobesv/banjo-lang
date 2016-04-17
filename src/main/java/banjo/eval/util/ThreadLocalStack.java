package banjo.eval.util;

import banjo.value.Value;
import fj.data.List;

public class ThreadLocalStack {

    public List<Value> stack = List.nil();

    public List<Value> stackPush(Value frame) {
        List<Value> oldStack = stack;
        stack = oldStack.cons(frame);
        return oldStack;
    }

    public List<Value> setStack(List<Value> newStack) {
        List<Value> oldStack = stack;
        stack = newStack;
        return oldStack;
    }

}
