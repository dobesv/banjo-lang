package banjo.expr.core;

import banjo.expr.token.Identifier;
import fj.P;
import fj.P2;
import fj.data.List;

public class TestAndExampleGatherer {

    public static final Identifier TESTS_KEY = new Identifier("unit tests");
    public static final Identifier EXAMPLES_KEY = new Identifier("usage examples");

    static public List<CoreExpr> findTests(CoreExpr base) {
    	return base.acceptVisitor(new BaseCoreExprVisitor<List<CoreExpr>>() {
    		@Override
    		public List<CoreExpr> fallback() {
    			return List.nil();
    		}
    
    		@Override
    		public List<CoreExpr> call(Call call) {
                if(call.target.eql(TESTS_KEY)) {
                    return List.join(call.args.map(arg -> arg.acceptVisitor(new BaseCoreExprVisitor<List<CoreExpr>>() {
                        @Override
                        public List<CoreExpr> fallback() {
                            return List.single(arg);
                        }
    
                        @Override
                        public List<CoreExpr> listLiteral(ListLiteral n) {
                            return n.elements;
                        }
    
                    })));
                }
    			return call.target.acceptVisitor(this).append(List.join(call.args.map(arg -> arg.acceptVisitor(this))));
    		}
    
    		@Override
    		public List<CoreExpr> listLiteral(
    				ListLiteral n) {
    			return List.join(n.getElements().<List<CoreExpr>>map(elt -> elt.acceptVisitor(this)));
    
    		}
    
    		public List<CoreExpr> slot(Slot slot) {
    			return slot.value.acceptVisitor(this);
    		}
    		@Override
    		public List<CoreExpr> objectLiteral(ObjectLiteral n) {
    			final List<List<CoreExpr>> methodExamples = n.getSlots().<List<CoreExpr>>map(this::slot);
    			return List.<CoreExpr>join(methodExamples);
    		}
    
    		public List<CoreExpr> binding(P2<Identifier,CoreExpr> binding) {
                if(binding._1().eql(TESTS_KEY) && binding._2() instanceof ListLiteral)
    				return ((ListLiteral)binding._2()).getElements();
    			return binding._2().acceptVisitor(this);
    		}
    		@Override
    		public List<CoreExpr> let(Let let) {
    		    List<List<CoreExpr>> examplesInBindings = let.bindings.map(this::binding);
    			List<CoreExpr> examplesInBody = let.body.acceptVisitor(this);
    			return List.join(examplesInBindings).append(examplesInBody).map(e -> new Let(let.getSourceFileRanges(), let.bindings, e));
    		}
    		@Override
    		public List<CoreExpr> extend(Extend n) {
    			return n.getBase().acceptVisitor(this).append(n.getExtension().acceptVisitor(this));
    		}
    
    		@Override
    		public List<CoreExpr> functionLiteral(FunctionLiteral f) {
    		    List<CoreExpr> result = f.body.acceptVisitor(this).map(e ->
    		        f.sourceObjectBinding.map(recId ->
    		            (CoreExpr)new Let(recId.getSourceFileRanges(), List.single(P.p(recId, f)), e))
    	            .orSome(e));
    			return result;
    		}
    
    		
    		@Override
    		public List<CoreExpr> projection(Projection projection) {
    		    return projection.object.acceptVisitor(this);
    		}

    	});
    }

    static public List<CoreExpr> findExamples(CoreExpr base) {
    	return base.acceptVisitor(new BaseCoreExprVisitor<List<CoreExpr>>() {
    		@Override
    		public List<CoreExpr> fallback() {
    			return List.nil();
    		}
    
    		@Override
    		public List<CoreExpr> call(Call call) {
    			return call.target.acceptVisitor(this).append(List.join(call.args.map(arg -> arg.acceptVisitor(this))));
    		}
    
    		@Override
    		public List<CoreExpr> listLiteral(
    				ListLiteral n) {
    			return List.join(n.getElements().<List<CoreExpr>>map(elt -> elt.acceptVisitor(this)));
    
    		}
    
    		public List<CoreExpr> slot(Slot slot) {
    			return slot.value.acceptVisitor(this);
    		}
    		@Override
    		public List<CoreExpr> objectLiteral(ObjectLiteral n) {
    			final List<List<CoreExpr>> methodExamples = n.getSlots().<List<CoreExpr>>map(this::slot);
    			return List.<CoreExpr>join(methodExamples);
    		}
    
    		public List<CoreExpr> binding(P2<Identifier,CoreExpr> binding) {
    			if(binding._1().eql(EXAMPLES_KEY) && binding._2() instanceof ListLiteral)
    				return ((ListLiteral)binding._2()).getElements();
    			return binding._2().acceptVisitor(this);
    		}
    		@Override
    		public List<CoreExpr> let(Let let) {
    		    List<List<CoreExpr>> examplesInBindings = let.bindings.map(this::binding);
    			List<CoreExpr> examplesInBody = let.body.acceptVisitor(this);
    			return List.join(examplesInBindings).append(examplesInBody).map(e -> new Let(let.getSourceFileRanges(), let.bindings, e));
    		}
    		@Override
    		public List<CoreExpr> extend(Extend n) {
    			return n.getBase().acceptVisitor(this).append(n.getExtension().acceptVisitor(this));
    		}
    
    		@Override
    		public List<CoreExpr> functionLiteral(FunctionLiteral f) {
    		    List<CoreExpr> result = f.body.acceptVisitor(this).map(e ->
    		        f.sourceObjectBinding.map(recId ->
    		            (CoreExpr)new Let(recId.getSourceFileRanges(), List.single(P.p(recId, f)), e))
    	            .orSome(e));
    			return result;
    		}
    
    		
    		@Override
    		public List<CoreExpr> projection(Projection projection) {
    		    return projection.object.acceptVisitor(this);
    		}
    		
    		
    	});
    }

    public static CoreExpr stripScope(CoreExpr x) {
    	while(x instanceof Let) {
    		x = ((Let)x).body;
    	}
    	return x;
    }

}
