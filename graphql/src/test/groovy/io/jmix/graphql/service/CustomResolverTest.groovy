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

package io.jmix.graphql.service


import io.jmix.graphql.AbstractGraphQLTest

class CustomResolverTest extends AbstractGraphQLTest {

    def "greeting service has greeting query"() {
        when:
        def response = query("service/greeting.gql")

        then:
        getBody(response) == '{"data":{' +
                '"greeting":{' +
                '"message":"Hello!"' +
                '}}}'
    }

    def "greeting service has personal greeting query"() {
        when:
        def response = query("service/personalGreeting.gql", asObjectNode('{"name": "Ivan"}'))

        then:
        getBody(response) == '{"data":{' +
                '"personalGreeting":{' +
                '"message":"Hello, Ivan!"' +
                '}}}'
    }

    def "greeting service shouldn't work with null argument"() {
        when:
        def response = query("service/personalGreeting.gql", asObjectNode('{"name": null}'))
        def error = getErrors(response)[0].getAsJsonObject()

        then:
        getMessage(error) == "Field 'name' of variable 'name' has coerced Null value for NonNull type 'String!'"
    }

    def "greeting service shouldn't contain meeting query"() {
        when:
        def response = query("service/meeting.gql")
        def error = getErrors(response)[0].getAsJsonObject()

        then:
        getMessage(error) == "Validation error of type FieldUndefined: " +
                "Field 'meeting' in type 'Query' is undefined @ 'meeting'"
    }
}
