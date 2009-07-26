package net.fortytwo.myotherbrain.flashmob.query;

import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.query.QueryException;

/**
 * Author: josh
 * Date: Jul 23, 2009
 * Time: 6:49:04 PM
 */
public abstract class FlashMOBQuery {
    public abstract handleResult(FirstClassItem subject,
                                 FirstClassItem source,
                                 float emphasis) throws QueryException;
}
