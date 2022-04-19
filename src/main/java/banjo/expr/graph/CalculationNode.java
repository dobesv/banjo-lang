package banjo.expr.graph;

/**
 * A transition does a computation.
 * @author dobes
 *
 */
public interface CalculationNode {

    /**
     * Observe the inputs of the node generically.
     * 
     * A transition with no inputs is either a constant or an argument.
     */
    ResultNode[] getArguments();

    /**
     * Observe the available outputs of the node generically
     */
    ResultNode[] getResults();

}
