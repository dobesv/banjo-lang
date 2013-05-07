package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import banjo.parser.errors.Problem;

public class Problematic<T> {
	private final List<Problem> problems;
	private final T value;

	public Problematic(T value, List<Problem> problems) {
		this.value = value;
		this.problems = nonNull(Collections.unmodifiableList(problems));
	}
	@SafeVarargs
	public Problematic(T value, Problem ... problems) {
		this(value, nonNull(problems.length == 0 ? Collections.<Problem>emptyList() : Arrays.asList(problems)));
	}

	public Problematic(T value, Problematic<?> left, Problematic<?> right) {
		this(value, ListUtil.concat(left.getProblems(), right.getProblems()));
	}
	public List<Problem> getProblems() {
		return this.problems;
	}

	public T getValue() {
		return this.value;
	}
	public Problematic<T> plusProblemsFrom(Problematic<T> p) {
		return withProblems(p.getProblems());
	}
	public Problematic<T> withProblems(List<Problem> problems) {
		if(problems.isEmpty())
			return this;
		return new Problematic<T>(this.value, ListUtil.concat(this.problems,problems));
	}
	public Problematic<T> with(Problem problem) {
		return new Problematic<T>(this.value, ListUtil.append(this.problems,problem));
	}

	/**
	 * Add all the problems in this one to the given collection, then return the value.
	 * @param problems
	 * @return
	 */
	public T dumpProblems(Collection<Problem> problems) {
		problems.addAll(this.problems);
		return this.value;
	}
}
