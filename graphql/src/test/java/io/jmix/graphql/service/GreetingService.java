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

package io.jmix.graphql.service;

import io.jmix.graphql.resolvers.CustomResolver;
import io.leangen.graphql.annotations.GraphQLArgument;
import io.leangen.graphql.annotations.GraphQLNonNull;
import io.leangen.graphql.spqr.spring.annotations.GraphQLApi;
import io.leangen.graphql.spqr.spring.annotations.WithResolverBuilder;
import org.springframework.stereotype.Component;

@Component("gql_ServiceForCustomResolver")
@GraphQLApi
@WithResolverBuilder(CustomResolver.class)
public class GreetingService {

    public Greeting getGreeting() {
        return new Greeting("Hello!");
    }

    public Greeting personalGreeting(@GraphQLNonNull @GraphQLArgument(name = "name") String name) {
        return new Greeting("Hello, " + name + "!");
    }

    public Greeting getMeeting() {
        return new Greeting("Hurry up");
    }
}

