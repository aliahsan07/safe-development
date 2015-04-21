/*******************************************************************************
    Copyright (c) 2015, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/
package kr.ac.kaist.jsaf.clone_refactor.model;

public interface IDeltaListener {
	public void add(DeltaEvent event);

	public void remove(DeltaEvent event);
}
