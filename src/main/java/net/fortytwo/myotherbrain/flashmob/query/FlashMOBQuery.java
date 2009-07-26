package net.fortytwo.myotherbrain.flashmob.query;

import net.fortytwo.myotherbrain.query.Handler;
import net.fortytwo.myotherbrain.query.QueryException;

/**
 * Author: josh
 * Date: Jul 23, 2009
 * Time: 6:49:04 PM
 */
public interface FlashMOBQuery {
    public void execute(Handler<FlashMOBQueryResult, QueryException> resultHandler,
                        FlashMOBQueryContext c) throws QueryException;
}
