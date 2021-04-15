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

package io.jmix.graphql.schema

import com.graphql.spring.boot.test.GraphQLResponse
import com.graphql.spring.boot.test.GraphQLTestTemplate
import io.jmix.graphql.AbstractGraphQLTest
import org.springframework.beans.factory.annotation.Autowired

class FilterIntegrationTest extends AbstractGraphQLTest {

    @Autowired
    GraphQLTestTemplate graphQLTestTemplate

    def "_eq for numbers"() {
        when:
        //where capacity = 50
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/eq/garage-eq-number.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","capacity":50},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","capacity":50' +
                '}]}}'
    }

    def "_eq for string with strict case"() {
        when:
        //where name = "Hillwood City"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/eq/garage-eq-string-strict-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","name":"Hillwood City"' +
                '}]}}'
    }

    def "_eq for string with ignore case"() {
        when:
        //where name = "hillwood city"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/eq/garage-eq-string-ignore-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[]}}'
    }

    def "_eq for boolean"() {
        when:
        //where vanEntry = false
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/eq/garage-eq-boolean.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","vanEntry":false},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","vanEntry":false},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","vanEntry":false}' +
                ']}}'
    }

    def "_eq for UUID"() {
        when:
        //where id = "bfe41616-f03d-f287-1397-8619f5dde390"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/eq/garage-eq-uuid.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390"}' +
                ']}}'
    }

    def "_neq for numbers"() {
        when:
        //where capacity <> 50
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/neq/garage-neq-number.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","capacity":20},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","capacity":21},' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","capacity":71},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","capacity":20},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","capacity":63},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","capacity":7},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","capacity":56},' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","capacity":9}' +
                ']}}'
    }

    def "_neq for string with strict case"() {
        when:
        //where name <> "Hillwood City"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/neq/garage-neq-string-strict-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","name":"Gerald Field"},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","name":"Roscoe\'s Funky Rags"},' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","name":"Chez Paris"},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","name":"The Fudge Place"},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","name":"The Fudge Place"},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","name":"Big Bob\'s Beeper Emporium"},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","name":"Sunset Arms"},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","name":"Watch Repair"},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","name":"P.S. 118"}' +
                ']}}'
    }

    def "_neq for string with ignore case"() {
        when:
        //where name <> "hillwood city"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/neq/garage-neq-string-ignore-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","name":"Gerald Field"},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","name":"Roscoe\'s Funky Rags"},' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","name":"Chez Paris"},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","name":"The Fudge Place"},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","name":"The Fudge Place"},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","name":"Big Bob\'s Beeper Emporium"},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","name":"Sunset Arms"},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","name":"Watch Repair"},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","name":"P.S. 118"},' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","name":"Hillwood City"}' +
                ']}}'
    }

    def "_neq for boolean"() {
        when:
        //where vanEntry <> false
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/neq/garage-neq-boolean.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","vanEntry":true},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","vanEntry":true},' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","vanEntry":true},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","vanEntry":true},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","vanEntry":true},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","vanEntry":true},' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","vanEntry":true}' +
                ']}}'
    }

    def "_neq for UUID"() {
        when:
        //where id <> "bfe41616-f03d-f287-1397-8619f5dde390"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/neq/garage-neq-uuid.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5"},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3"},' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948"},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044"},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d"},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264"},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d"},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620"},' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12"}' +
                ']}}'
    }

    def "_gt for numbers"() {
        when:
        //where capacity > 50
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/gt/garage-gt-number.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","capacity":71},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","capacity":63},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","capacity":56}]}}'
    }

    def "_gte for numbers"() {
        when:
        //where capacity >= 50
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/gte/garage-gte-number.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","capacity":71},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","capacity":63},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","capacity":50},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","capacity":50},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","capacity":56}' +
                ']}}'
    }

    def "_lt for numbers"() {
        when:
        //where capacity < 50
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/lt/garage-lt-number.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","capacity":20},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","capacity":21},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","capacity":20},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","capacity":7},' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","capacity":9}' +
                ']}}'
    }

    def "_lte for numbers"() {
        when:
        //where capacity <= 50
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/lte/garage-lte-number.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","capacity":20},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","capacity":21},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","capacity":20},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","capacity":50},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","capacity":50},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","capacity":7},' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","capacity":9}' +
                ']}}'
    }

    def "_contains for string with strict case"() {
        when:
        //where name like "%Hillwood%"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/contains/garage-contains-string-strict-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","name":"Hillwood City"' +
                '}]}}'
    }

    def "_contains for string with ignore case"() {
        when:
        //where name like "%hillwood%"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/contains/garage-contains-string-ignore-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","name":"Hillwood City"' +
                '}]}}'
    }

    def "_notContains for string with strict case"() {
        when:
        //where name not like "%Hillwood%"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/not_contains/garage-not-contains-string-strict-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","name":"Gerald Field"},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","name":"Roscoe\'s Funky Rags"},' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","name":"Chez Paris"},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","name":"The Fudge Place"},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","name":"The Fudge Place"},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","name":"Big Bob\'s Beeper Emporium"},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","name":"Sunset Arms"},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","name":"Watch Repair"},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","name":"P.S. 118"}' +
                ']}}'
    }

    def "_notContains for string with ignore case"() {
        when:
        //where name not like "%hillwood%"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/not_contains/garage-not-contains-string-ignore-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","name":"Gerald Field"},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","name":"Roscoe\'s Funky Rags"},' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","name":"Chez Paris"},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","name":"The Fudge Place"},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","name":"The Fudge Place"},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","name":"Big Bob\'s Beeper Emporium"},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","name":"Sunset Arms"},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","name":"Watch Repair"},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","name":"P.S. 118"}' +
                ']}}'
    }

    def "_startsWith for string with strict case"() {
        when:
        //where name like "Hil%"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/starts_with/garage-starts-with-string-strict-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","name":"Hillwood City"}' +
                ']}}'
    }

    def "_startsWith for string with ignore case"() {
        when:
        //where name like "hil%"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/starts_with/garage-starts-with-string-ignore-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","name":"Hillwood City"}' +
                ']}}'
    }

    def "_endsWith for string with strict case"() {
        when:
        //where name like "%ity"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/ends_with/garage-ends-with-string-strict-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","name":"Hillwood City"}' +
                ']}}'
    }

    def "_endsWith for string with ignore case"() {
        when:
        //where name like "%ITY"
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/ends_with/garage-ends-with-string-ignore-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","name":"Hillwood City"}' +
                ']}}'
    }

    def "_in for numbers"() {
        when:
        //where capacity in (50, 21, 7)
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/in/garage-in-number.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","capacity":21},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","capacity":50},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","capacity":50},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","capacity":7}' +
                ']}}'
    }

    def "_in for string with strict case"() {
        when:
        //where name in ("Hillwood City", "Chez Paris")
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/in/garage-in-string-strict-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","name":"Chez Paris"},' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","name":"Hillwood City"}' +
                ']}}'
    }

    def "_in for string with ignore case"() {
        when:
        //where name in ("hillwood city")
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/in/garage-in-string-ignore-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[]}}'
    }

    def "_in for UUID"() {
        when:
        //where id in ("bfe41616-f03d-f287-1397-8619f5dde390", "2094170e-5739-43bd-ed5c-783c949c9948")
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/in/garage-in-uuid.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948"},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390"}' +
                ']}}'
    }

    def "_notIn for numbers"() {
        when:
        //where capacity not in (50, 21, 7)
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/not_in/garage-not-in-number.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","capacity":20},' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","capacity":71},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","capacity":20},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","capacity":63},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","capacity":56},' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","capacity":9}' +
                ']}}'
    }

    def "_notIn for string with strict case"() {
        when:
        //where name not in ("Hillwood City", "Chez Paris")
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/not_in/garage-not-in-string-strict-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","name":"Gerald Field"},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","name":"Roscoe\'s Funky Rags"},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","name":"The Fudge Place"},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","name":"The Fudge Place"},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","name":"Big Bob\'s Beeper Emporium"},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","name":"Sunset Arms"},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","name":"Watch Repair"},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","name":"P.S. 118"}' +
                ']}}'
    }

    def "_notIn for string with ignore case"() {
        when:
        //where name not in ("hillwood city", "chez paris")
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/not_in/garage-not-in-string-ignore-case.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5","name":"Gerald Field"},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3","name":"Roscoe\'s Funky Rags"},' +
                '{"id":"2094170e-5739-43bd-ed5c-783c949c9948","name":"Chez Paris"},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044","name":"The Fudge Place"},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d","name":"The Fudge Place"},' +
                '{"id":"bfe41616-f03d-f287-1397-8619f5dde390","name":"Big Bob\'s Beeper Emporium"},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264","name":"Sunset Arms"},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d","name":"Watch Repair"},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620","name":"P.S. 118"},' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12","name":"Hillwood City"}' +
                ']}}'
    }

    def "_notIn for UUID"() {
        when:
        //where id not in ("bfe41616-f03d-f287-1397-8619f5dde390", "2094170e-5739-43bd-ed5c-783c949c9948")
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/not_in/garage-not-in-uuid.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_GarageList":[' +
                '{"id":"18a4b0b4-b7b5-da87-e5d8-c8f02e97eda5"},' +
                '{"id":"1e3cb465-c0d8-1f31-4231-08c34e101fc3"},' +
                '{"id":"4e0ba898-74e4-8ab7-58fc-044364221044"},' +
                '{"id":"b79e6fc9-f07a-d5cd-e072-8104a5d5101d"},' +
                '{"id":"ca83fc1c-95e5-d012-35bf-151b7f720264"},' +
                '{"id":"d881e37a-d28a-4e48-cb96-668d4a6fb57d"},' +
                '{"id":"d99d468e-3cc0-01da-295e-595e48fec620"},' +
                '{"id":"ff01c573-ebf3-c704-3ad0-fd582f7a2a12"}' +
                ']}}'
    }

    def "_isNull (true) for numbers"() {
        when:
        //where capacity is null
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/is_null/datatypesTestEntity-is-null-number.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_DatatypesTestEntityList":[' +
                '{"id":"00000000-0000-0000-0000-000000000000","integerAttr":null}' +
                ']}}'
    }

    def "_isNull (true) for string"() {
        when:
        //where name is null
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/is_null/car-is-null-string.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_CarList":[' +
                '{"id":"fc63ccfc-e8e9-5486-5c38-98ae42f729da","model":null}' +
                ']}}'
    }

    def "_isNull (true) for boolean"() {
        when:
        //where vanEntry is null
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/is_null/datatypesTestEntity-is-null-boolean.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_DatatypesTestEntityList":[' +
                '{"id":"00000000-0000-0000-0000-000000000000","booleanAttr":null},' +
                '{"id":"db9faa31-dfa3-4b97-943c-ba268888cdc3","booleanAttr":null}' +
                ']}}'
    }

    def "_isNull (true) for UUID"() {
        when:
        //where id is null
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/is_null/datatypesTestEntity-is-null-uuid.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_DatatypesTestEntityList":[' +
                '{"id":"00000000-0000-0000-0000-000000000000","uuidAttr":null}' +
                ']}}'
    }

    def "_isNull for date"() {
        when:
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/is_null/car-is-null-date.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_CarList":[' +
                '{"id":"c5a0c22e-a8ce-4c5a-9068-8fb142af26ae","lastModifiedDate":null}' +
                ']}}'
    }

    def "_isNull (false) for numbers"() {
        when:
        //where capacity is not null
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/is_null/datatypesTestEntity-is-null-false-number.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_DatatypesTestEntityList":[' +
                '{"id":"032fd8a5-e042-4828-a802-36cbd2ce12de","integerAttr":10482},' +
                '{"id":"b1a1f3c9-6076-4725-8c4a-65a4267d15e1","integerAttr":304221},' +
                '{"id":"db9faa31-dfa3-4b97-943c-ba268888cdc3","integerAttr":0}' +
                ']}}'
    }

    def "_isNull (false) for string"() {
        when:
        //where name is not null
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/is_null/car-is-null-false-string.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_CarList":[' +
                '{"id":"c5a0c22e-a8ce-4c5a-9068-8fb142af26ae","model":"m14"},' +
                '{"id":"aa595879-484f-4e7d-b19a-429cb2d84f79","model":"m07"},' +
                '{"id":"7db61cfc-1e50-4898-a76d-42347ffb763f","model":"m03"},' +
                '{"id":"8561ba7a-49c5-4683-9251-59f376018a89","model":"m06"},' +
                '{"id":"c7052489-3697-48f6-a0f3-8e874d732865","model":"m02"},' +
                '{"id":"b94eede4-c1da-43df-830d-36ef1414385b","model":"m01"},' +
                '{"id":"bc5b3371-7418-4c79-90e8-81b09c59d9a1","model":"m04"},' +
                '{"id":"6b853033-db8c-4d51-ab4c-4b3146796348","model":"m13"},' +
                '{"id":"2325c7af-9569-4f66-bcf7-bb52cba5388b","model":"m05"},' +
                '{"id":"bf6791e6-0e0a-8ca1-6a98-75b0a8971676","model":"X0"},' +
                '{"id":"5f14d58d-6f24-4590-eef9-4b5885ed3e34","model":"968M"},' +
                '{"id":"c4ef4c14-5be9-406a-8457-db0bc760913a","model":"m12"},' +
                '{"id":"c2a14bec-cd7d-a3e4-1581-db243cf704aa","model":"911"},' +
                '{"id":"a64e6ef7-49d6-4ce5-8973-8c95ac1576e0","model":"m11"},' +
                '{"id":"50277e41-97d1-4af2-a122-1e87ae3011d9","model":"m10"},' +
                '{"id":"3da61043-aaad-7e30-c7f5-c1f1328d3980","model":"2121"},' +
                '{"id":"63e88502-3cf0-382c-8f5f-07a0c8a4d9b2","model":"2410"},' +
                '{"id":"5db1dce7-ceee-42f8-a14b-ddb93c4ad999","model":"m09"},' +
                '{"id":"f44d486f-2fa3-4789-d02a-c1d2b2c67fc6","model":"Model Y"},' +
                '{"id":"73c05bf0-ef67-4291-48a2-1481fc7f17e6","model":"2141"},' +
                '{"id":"94505084-e12c-44c0-9e55-0ee9ef5f3a90","model":"m08"}' +
                ']}}'
    }

    def "_isNull (false) for boolean"() {
        when:
        //where vanEntry is not null
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/is_null/datatypesTestEntity-is-null-false-boolean.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_DatatypesTestEntityList":[' +
                '{"id":"032fd8a5-e042-4828-a802-36cbd2ce12de","booleanAttr":true},' +
                '{"id":"b1a1f3c9-6076-4725-8c4a-65a4267d15e1","booleanAttr":false}' +
                ']}}'
    }

    def "_isNull (false) for UUID"() {
        when:
        //where id is not null
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/is_null/datatypesTestEntity-is-null-false-uuid.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_DatatypesTestEntityList":[' +
                '{"id":"032fd8a5-e042-4828-a802-36cbd2ce12de","uuidAttr":"c6a1cee6-f562-48a0-acbe-9625e0b278b1"},' +
                '{"id":"b1a1f3c9-6076-4725-8c4a-65a4267d15e1","uuidAttr":"9b4188bf-c382-4b89-aedf-b6bcee6f2f76"},' +
                '{"id":"db9faa31-dfa3-4b97-943c-ba268888cdc3","uuidAttr":"db9faa31-dfa3-4b97-943c-ba268888cdc3"}' +
                ']}}'
    }

    def "_isNull (false) for date"() {
        when:
        def response = graphQLTestTemplate.postForResource(
                "graphql/filter_integration_test/is_null/car-is-null-false-date.graphql"
        )

        then:
        getBody(response) == '{"data":{"scr_CarList":[' +
                '{"id":"aa595879-484f-4e7d-b19a-429cb2d84f79","lastModifiedDate":"2021-04-06T20:00:00Z"},' +
                '{"id":"7db61cfc-1e50-4898-a76d-42347ffb763f","lastModifiedDate":"2021-03-30T20:00:00Z"},' +
                '{"id":"8561ba7a-49c5-4683-9251-59f376018a89","lastModifiedDate":"2021-03-04T20:00:00Z"},' +
                '{"id":"c7052489-3697-48f6-a0f3-8e874d732865","lastModifiedDate":"2021-02-28T20:00:00Z"},' +
                '{"id":"b94eede4-c1da-43df-830d-36ef1414385b","lastModifiedDate":"2021-02-27T20:00:00Z"},' +
                '{"id":"bc5b3371-7418-4c79-90e8-81b09c59d9a1","lastModifiedDate":"2021-02-20T20:00:00Z"},' +
                '{"id":"6b853033-db8c-4d51-ab4c-4b3146796348","lastModifiedDate":"2021-02-13T20:00:00Z"},' +
                '{"id":"2325c7af-9569-4f66-bcf7-bb52cba5388b","lastModifiedDate":"2021-02-10T20:00:00Z"},' +
                '{"id":"bf6791e6-0e0a-8ca1-6a98-75b0a8971676","lastModifiedDate":"2021-01-30T20:00:00Z"},' +
                '{"id":"5f14d58d-6f24-4590-eef9-4b5885ed3e34","lastModifiedDate":"2021-01-20T20:00:00Z"},' +
                '{"id":"c4ef4c14-5be9-406a-8457-db0bc760913a","lastModifiedDate":"2021-01-17T20:00:00Z"},' +
                '{"id":"c2a14bec-cd7d-a3e4-1581-db243cf704aa","lastModifiedDate":"2021-01-10T20:00:00Z"},' +
                '{"id":"a64e6ef7-49d6-4ce5-8973-8c95ac1576e0","lastModifiedDate":"2021-01-08T20:00:00Z"},' +
                '{"id":"50277e41-97d1-4af2-a122-1e87ae3011d9","lastModifiedDate":"2021-01-05T20:00:00Z"},' +
                '{"id":"3da61043-aaad-7e30-c7f5-c1f1328d3980","lastModifiedDate":"2021-01-02T20:00:00Z"},' +
                '{"id":"63e88502-3cf0-382c-8f5f-07a0c8a4d9b2","lastModifiedDate":"2020-12-31T20:00:00Z"},' +
                '{"id":"fc63ccfc-e8e9-5486-5c38-98ae42f729da","lastModifiedDate":"2020-12-30T20:00:00Z"},' +
                '{"id":"5db1dce7-ceee-42f8-a14b-ddb93c4ad999","lastModifiedDate":"2020-11-30T20:00:00Z"},' +
                '{"id":"f44d486f-2fa3-4789-d02a-c1d2b2c67fc6","lastModifiedDate":"2020-10-31T20:00:00Z"},' +
                '{"id":"73c05bf0-ef67-4291-48a2-1481fc7f17e6","lastModifiedDate":"2020-09-30T20:00:00Z"},' +
                '{"id":"94505084-e12c-44c0-9e55-0ee9ef5f3a90","lastModifiedDate":"2020-06-11T20:00:00Z"}' +
                ']}}'
    }

    private static String getBody(GraphQLResponse response) {
        return response.rawResponse.body
    }
}
