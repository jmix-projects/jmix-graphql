/*
 * Copyright 2021 Haulmont.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.jmix.graphql.updater;

import io.jmix.core.FetchPlan;
import io.jmix.core.LoadContext;
import io.jmix.core.metamodel.model.MetaClass;

public class GraphQLUpsertResultGetterContext<E> {

    private MetaClass metaClass;
    private Object entity;
    private LoadContext<E> loadContext;
    private FetchPlan fetchPlan;

    public GraphQLUpsertResultGetterContext(MetaClass metaClass, Object entity, LoadContext<E> loadContext, FetchPlan fetchPlan) {
        this.metaClass = metaClass;
        this.entity = entity;
        this.loadContext = loadContext;
        this.fetchPlan = fetchPlan;
    }

    public MetaClass getMetaClass() {
        return metaClass;
    }

    public void setMetaClass(MetaClass metaClass) {
        this.metaClass = metaClass;
    }

    public Object getEntity() {
        return entity;
    }

    public void setEntity(Object id) {
        this.entity = id;
    }

    public LoadContext<E> getLoadContext() {
        return loadContext;
    }

    public void setLoadContext(LoadContext<E> loadContext) {
        this.loadContext = loadContext;
    }

    public FetchPlan getFetchPlan() {
        return fetchPlan;
    }

    public void setFetchPlan(FetchPlan fetchPlan) {
        this.fetchPlan = fetchPlan;
    }

}
