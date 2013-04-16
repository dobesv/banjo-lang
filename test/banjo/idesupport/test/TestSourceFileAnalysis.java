package banjo.idesupport.test;

import static banjo.dom.test.ParseTestUtils.test;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Test;

import banjo.dom.Comment;
import banjo.dom.Ellipsis;
import banjo.dom.Expr;
import banjo.dom.HasFileRange;
import banjo.dom.Identifier;
import banjo.dom.NumberLiteral;
import banjo.dom.ObjectLiteral;
import banjo.dom.OperatorRef;
import banjo.dom.StringLiteral;
import banjo.dom.TokenVisitor;
import banjo.dom.UnitRef;
import banjo.dom.Whitespace;
import banjo.idesupport.SourceFileAnalysis;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoScanner;
import banjo.parser.util.TokenCollector;

public class TestSourceFileAnalysis {

	
}
