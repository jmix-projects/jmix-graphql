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
import io.jmix.graphql.annotation.GraphQLLoader;
import io.jmix.graphql.loader.GraphQLEntityCountLoader;
import io.jmix.graphql.loader.GraphQLEntityCountLoaderContext;
import io.jmix.graphql.loader.GraphQLEntityListLoader;
import io.jmix.graphql.loader.GraphQLEntityLoader;
import io.jmix.graphql.loader.GraphQLEntityLoaderContext;
import io.jmix.graphql.loader.GraphQLListLoaderContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import test_support.entity.Car;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Component
@GraphQLLoader
public class CarLoader implements GraphQLEntityCountLoader<Car>,
        GraphQLEntityListLoader<Car>, GraphQLEntityLoader<Car> {

    @Autowired
    Metadata metadata;

    @Override
    public Long loadCount(GraphQLEntityCountLoaderContext<Car> context) {
        return 999L;
    }

    @Override
    public List<Car> loadEntityList(GraphQLListLoaderContext<Car> context) {
        Car car1 = metadata.create(Car.class);
        car1.setManufacturer("BMW");
        car1.setModel("M3");
        car1.setPrice(BigDecimal.valueOf(10));
        Car car2 = metadata.create(Car.class);
        car2.setManufacturer("Lada");
        car2.setModel("Vesta");
        car2.setPrice(BigDecimal.valueOf(20));
        return new ArrayList<>(Arrays.asList(car1, car2));
    }

    @Override
    public Car loadEntity(GraphQLEntityLoaderContext<Car> context) {
        Car car = metadata.create(Car.class);
        car.setManufacturer("Lada");
        car.setModel("Vesta");
        car.setPrice(BigDecimal.valueOf(10));
        return car;
    }
}
