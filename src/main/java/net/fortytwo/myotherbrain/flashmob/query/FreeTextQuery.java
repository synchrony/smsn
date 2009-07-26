package net.fortytwo.myotherbrain.flashmob.query;

import net.fortytwo.myotherbrain.flashmob.model.FlashMOBFirstClassItem;
import net.fortytwo.myotherbrain.query.WeightedValue;
import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.query.FreeTextQueries;
import net.fortytwo.myotherbrain.query.Handler;
import net.fortytwo.myotherbrain.query.QueryException;

/**
 * Author: josh
 * Date: Jul 23, 2009
 * Time: 6:53:59 PM
 */
public class FreeTextQuery implements FlashMOBQuery {
    private String queryExpression;

    public String getQueryExpression() {
        return queryExpression;
    }

    public void setQueryExpression(String queryExpression) {
        this.queryExpression = queryExpression;
    }

    public void execute(final Handler<FlashMOBQueryResult, QueryException> resultHandler,
                        final FlashMOBQueryContext c) throws QueryException {
        Handler<WeightedValue<FirstClassItem>, QueryException> handler
                = new Handler<WeightedValue<FirstClassItem>, QueryException>() {
            public boolean handle(final WeightedValue<FirstClassItem> value) throws QueryException {
                FirstClassItem item = value.value;

                if (c.isVisible(item)) {
                    FlashMOBFirstClassItem bean = c.toBean(item);

                    FlashMOBQueryResult r = new FlashMOBQueryResult();
                    r.setSubject(bean);
                    r.setEmphasis((float) value.weight);
                    // FIXME
                    r.setSource(null);

                    return resultHandler.handle(r);
                } else {
                    return true;
                }
            }
        };

        FreeTextQueries.executeFreetextSearch(queryExpression, handler, c.getModelConnection());
    }
}
