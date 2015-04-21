package kr.ac.kaist.jsaf.clone_refactor.core;

import org.eclipse.wst.jsdt.core.dom.ASTNode;
import org.eclipse.wst.jsdt.core.dom.FieldAccess;
import org.eclipse.wst.jsdt.core.dom.FunctionInvocation;
import org.eclipse.wst.jsdt.core.dom.InfixExpression;
import org.eclipse.wst.jsdt.core.dom.NumberLiteral;
import org.eclipse.wst.jsdt.core.dom.PrefixExpression;
import org.eclipse.wst.jsdt.core.dom.SimpleName;
import org.eclipse.wst.jsdt.core.dom.StringLiteral;

public class NodeMatcher {
	public static boolean match(ASTNode node1, ASTNode node2) {
		if (node1 instanceof FieldAccess && node2 instanceof FieldAccess)
			return match((FieldAccess) node1, (FieldAccess) node2);
		else if (node1 instanceof FunctionInvocation
				&& node2 instanceof FunctionInvocation)
			return match((FunctionInvocation) node1, (FunctionInvocation) node2);
		else if (node1 instanceof InfixExpression
				&& node2 instanceof InfixExpression)
			return match((InfixExpression) node1, (InfixExpression) node2);
		else if (node1 instanceof NumberLiteral
				&& node2 instanceof NumberLiteral)
			return match((NumberLiteral) node1, (NumberLiteral) node2);
		else if (node1 instanceof PrefixExpression
				&& node2 instanceof PrefixExpression)
			return match((PrefixExpression) node1, (PrefixExpression) node2);
		else if (node1 instanceof StringLiteral
				&& node2 instanceof StringLiteral)
			return match((StringLiteral) node1, (StringLiteral) node2);
		else
			return true;
	}

	public static boolean match(final FieldAccess node1, final FieldAccess node2) {
		return node1.getName().getIdentifier()
				.equals(node2.getName().getIdentifier());
	}

	public static boolean match(final FunctionInvocation node1,
			final FunctionInvocation node2) {
		return node1.getName().getIdentifier()
				.equals(node2.getName().getIdentifier());
	}

	public static boolean match(final InfixExpression node1,
			final InfixExpression node2) {
		return node1.getOperator().toString()
				.equals(node2.getOperator().toString());
	}

	public static boolean match(final NumberLiteral node1,
			final NumberLiteral node2) {
		return node1.getToken().equals(node2.getToken());
	}

	public static boolean match(final PrefixExpression node1,
			final PrefixExpression node2) {
		return node1.getOperator().toString()
				.equals(node2.getOperator().toString());
	}

	public static boolean match(final SimpleName node1, final SimpleName node2) {
		return node1.getIdentifier().equals(node2.getIdentifier());
	}

	public static boolean match(final StringLiteral node1,
			final StringLiteral node2) {
		return node1.getLiteralValue().equals(node2.getLiteralValue());
	}
}
