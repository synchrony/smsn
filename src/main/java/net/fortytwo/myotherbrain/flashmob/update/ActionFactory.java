package net.fortytwo.myotherbrain.flashmob.update;

import net.fortytwo.myotherbrain.flashmob.update.actions.AddAliasBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.AddMarkerTagBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.BreakAssociationBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.CreateAssociationBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.CreateFirstClassItemBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.CreateGeoPointBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.CreateLiteralBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.CreateWebResourceBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.RemoveAliasBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.RemoveMarkerTagBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.SetDescriptionBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.SetEmphasisBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.SetIconBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.SetNameBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.SetSensitivityBean;
import net.fortytwo.myotherbrain.flashmob.update.actions.ActionBean;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import net.fortytwo.myotherbrain.update.UpdateException;
import net.fortytwo.myotherbrain.update.actions.AddAlias;
import net.fortytwo.myotherbrain.update.actions.AddMarkerTag;
import net.fortytwo.myotherbrain.update.actions.BreakAssociation;
import net.fortytwo.myotherbrain.update.actions.CreateAssociation;
import net.fortytwo.myotherbrain.update.actions.CreateAtom;
import net.fortytwo.myotherbrain.update.actions.CreateGeoPoint;
import net.fortytwo.myotherbrain.update.actions.CreateLiteral;
import net.fortytwo.myotherbrain.update.actions.CreateWebResource;
import net.fortytwo.myotherbrain.update.actions.RemoveAlias;
import net.fortytwo.myotherbrain.update.actions.RemoveMarkerTag;
import net.fortytwo.myotherbrain.update.actions.SetDescription;
import net.fortytwo.myotherbrain.update.actions.SetEmphasis;
import net.fortytwo.myotherbrain.update.actions.SetIcon;
import net.fortytwo.myotherbrain.update.actions.SetName;
import net.fortytwo.myotherbrain.update.actions.SetSensitivity;

import java.util.Map;
import java.util.HashMap;
import java.net.URI;
import java.net.URISyntaxException;

/**
 * Author: josh
 * Date: Jul 11, 2009
 * Time: 3:36:49 PM
 */
public class ActionFactory {
    private static final Map<Class, ActionBeanHandler> actionBeanHandlers;

    static {
        actionBeanHandlers = new HashMap<Class, ActionBeanHandler>();

        actionBeanHandlers.put(AddAliasBean.class, new ActionBeanHandler<AddAliasBean>() {
            public WriteAction createAction(AddAliasBean bean, WriteContext c) throws UpdateException {
                return new AddAlias(toURI(bean.getSubject()),
                        toURI(bean.getNewAlias()),
                        c);
            }
        });

        actionBeanHandlers.put(AddMarkerTagBean.class, new ActionBeanHandler<AddMarkerTagBean>() {
            public WriteAction createAction(AddMarkerTagBean bean, WriteContext c) throws UpdateException {
                return new AddMarkerTag(toURI(bean.getSubject()),
                        toURI(bean.getNewMarkerTag()),
                        c);
            }
        });

        actionBeanHandlers.put(BreakAssociationBean.class, new ActionBeanHandler<BreakAssociationBean>() {
            public WriteAction createAction(BreakAssociationBean bean, WriteContext c) throws UpdateException {
                return new BreakAssociation(toURI(bean.getSubject()),
                        c);
            }
        });

        actionBeanHandlers.put(CreateAssociationBean.class, new ActionBeanHandler<CreateAssociationBean>() {
            public WriteAction createAction(CreateAssociationBean bean, WriteContext c) throws UpdateException {
                return new CreateAssociation(toURI(bean.getSubject()),
                        bean.getName(),
                        bean.getDescription(),
                        bean.getRichTextDescription(),
                        toURI(bean.getIcon()),
                        toURI(bean.getSensitivity()),
                        bean.getEmphasis(),
                        bean.getCreationTimeStamp(),
                        toURI(bean.getCreationPlaceStamp()),
                        toURI(bean.getAssociationSubject()),
                        toURI(bean.getAssociationObject()),
                        c);
            }
        });

        actionBeanHandlers.put(CreateFirstClassItemBean.class, new ActionBeanHandler<CreateFirstClassItemBean>() {
            public WriteAction createAction(CreateFirstClassItemBean bean, WriteContext c) throws UpdateException {
                return new CreateAtom(toURI(bean.getSubject()),
                        bean.getName(),
                        bean.getDescription(),
                        bean.getRichTextDescription(),
                        toURI(bean.getIcon()),
                        toURI(bean.getSensitivity()),
                        bean.getEmphasis(),
                        bean.getCreationTimeStamp(),
                        toURI(bean.getCreationPlaceStamp()),
                        c);
            }
        });

        actionBeanHandlers.put(CreateGeoPointBean.class, new ActionBeanHandler<CreateGeoPointBean>() {
            public WriteAction createAction(CreateGeoPointBean bean, WriteContext c) throws UpdateException {
                return new CreateGeoPoint(toURI(bean.getSubject()),
                        bean.getLongitude(),
                        bean.getLatitude(),
                        c);
            }
        });

        actionBeanHandlers.put(CreateLiteralBean.class, new ActionBeanHandler<CreateLiteralBean>() {
            public WriteAction createAction(CreateLiteralBean bean, WriteContext c) throws UpdateException {
                return new CreateLiteral(toURI(bean.getSubject()),
                        bean.getName(),
                        bean.getDescription(),
                        bean.getRichTextDescription(),
                        toURI(bean.getIcon()),
                        toURI(bean.getSensitivity()),
                        bean.getEmphasis(),
                        bean.getCreationTimeStamp(),
                        toURI(bean.getCreationPlaceStamp()),
                        bean.getLexicalForm(),
                        toURI(bean.getDatatypeURI()),
                        bean.getLanguageTag(),
                        c);
            }
        });

        actionBeanHandlers.put(CreateWebResourceBean.class, new ActionBeanHandler<CreateWebResourceBean>() {
            public WriteAction createAction(CreateWebResourceBean bean, WriteContext c) throws UpdateException {
                return new CreateWebResource(toURI(bean.getSubject()),
                        bean.getRepresentationMediaType(),
                        bean.getRepresentationSha1Sum(),
                        c);
            }
        });

        actionBeanHandlers.put(RemoveAliasBean.class, new ActionBeanHandler<RemoveAliasBean>() {
            public WriteAction createAction(RemoveAliasBean bean, WriteContext c) throws UpdateException {
                return new RemoveAlias(toURI(bean.getSubject()),
                        toURI(bean.getTargetAlias()),
                        c);
            }
        });

        actionBeanHandlers.put(RemoveMarkerTagBean.class, new ActionBeanHandler<RemoveMarkerTagBean>() {
            public WriteAction createAction(RemoveMarkerTagBean bean, WriteContext c) throws UpdateException {
                return new RemoveMarkerTag(toURI(bean.getSubject()),
                        toURI(bean.getTargetMarkerTag()),
                        c);
            }
        });

        actionBeanHandlers.put(SetDescriptionBean.class, new ActionBeanHandler<SetDescriptionBean>() {
            public WriteAction createAction(SetDescriptionBean bean, WriteContext c) throws UpdateException {
                return new SetDescription(toURI(bean.getSubject()),
                        bean.getDescription(),
                        bean.getRichTextDescription(),
                        c);
            }
        });

        actionBeanHandlers.put(SetEmphasisBean.class, new ActionBeanHandler<SetEmphasisBean>() {
            public WriteAction createAction(SetEmphasisBean bean, WriteContext c) throws UpdateException {
                return new SetEmphasis(toURI(bean.getSubject()),
                        bean.getEmphasis(),
                        c);
            }
        });

        actionBeanHandlers.put(SetIconBean.class, new ActionBeanHandler<SetIconBean>() {
            public WriteAction createAction(SetIconBean bean, WriteContext c) throws UpdateException {
                return new SetIcon(toURI(bean.getSubject()),
                        toURI(bean.getIcon()),
                        c);
            }
        });

        actionBeanHandlers.put(SetNameBean.class, new ActionBeanHandler<SetNameBean>() {
            public WriteAction createAction(SetNameBean bean, WriteContext c) throws UpdateException {
                return new SetName(toURI(bean.getSubject()),
                        bean.getName(),
                        c);
            }
        });

        actionBeanHandlers.put(SetSensitivityBean.class, new ActionBeanHandler<SetSensitivityBean>() {
            public WriteAction createAction(SetSensitivityBean bean, WriteContext c) throws UpdateException {
                return new SetSensitivity(toURI(bean.getSubject()),
                        toURI(bean.getSensitivity()),
                        c);
            }
        });
    }

    //private <T extends ActionBean> void addHandler(final ActionBeanHandler<T> handler) {
    //    actionBeanHandlers.put(T.class, handler);
    //}

    public static <T extends ActionBean> WriteAction createAction(final T bean,
                                                                  final WriteContext c) throws UpdateException {
        ActionBeanHandler<T> handler = (ActionBeanHandler<T>) actionBeanHandlers.get(bean.getClass());
        return handler.createAction(bean, c);
    }

    private static URI toURI(final String s) {
        try {
            return null == s ? null : new URI(s);
        } catch (URISyntaxException e) {
            throw new IllegalArgumentException(e);
        }
    }

    private interface ActionBeanHandler<T extends ActionBean> {
        WriteAction createAction(T bean,
                                 WriteContext c) throws UpdateException;
    }
}
