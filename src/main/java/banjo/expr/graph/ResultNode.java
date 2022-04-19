package banjo.expr.graph;

public interface ResultNode {

    /**
     * Get the transition that produces this value
     */
    CalculationNode getProducer();
}
