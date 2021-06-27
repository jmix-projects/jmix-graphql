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

package io.jmix.graphql.custom.service;

import io.jmix.core.Metadata;
import io.jmix.graphql.annotation.GraphQLModifier;
import io.jmix.graphql.modifier.GraphQLEntityRemover;
import io.jmix.graphql.modifier.GraphQLEntityRemoverContext;
import io.jmix.graphql.modifier.GraphQLUpdater;
import io.jmix.graphql.modifier.GraphQLUpdaterContext;
import io.jmix.graphql.modifier.GraphQLUpsertResultGetter;
import io.jmix.graphql.modifier.GraphQLUpsertResultGetterContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import test_support.entity.Car;
import test_support.entity.CarType;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

@Component
@GraphQLModifier
public class CarModifier implements GraphQLEntityRemover<Car>, GraphQLUpdater<Car>, GraphQLUpsertResultGetter<Car> {
    @Autowired
    Metadata metadata;

    private static Logger log = LoggerFactory.getLogger(CarModifier.class);

    @Override
    public Collection<Car> importEntities(GraphQLUpdaterContext<Car> context) {
        context.getEntities().get(0).setPrice(BigDecimal.valueOf(10));
        return context.getEntities();
    }

    @Override
    public Car getUpsertResult(GraphQLUpsertResultGetterContext<Car> context) {
        context.getEntity().setMaxPassengers(5);
        return context.getEntity();
    }

    @Override
    public void deleteEntity(GraphQLEntityRemoverContext<Car> graphQLEntityRemoverContext) {
        log.warn("Delete entity with id " + graphQLEntityRemoverContext.getId());
    }
}
