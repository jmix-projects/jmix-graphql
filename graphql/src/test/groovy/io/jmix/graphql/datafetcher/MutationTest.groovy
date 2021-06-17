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

package io.jmix.graphql.datafetcher

import io.jmix.graphql.AbstractGraphQLTest
import org.springframework.test.annotation.DirtiesContext
import org.springframework.test.context.TestPropertySource
import spock.lang.Ignore
import test_support.entity.CarType

@DirtiesContext
@TestPropertySource(properties = [
        "jmix.security.oauth2.devMode=true",
        "jmix.security.oauth2.devUsername=admin"
])
class MutationTest extends AbstractGraphQLTest {

    def id1 = "4c34985b-67be-4788-891a-839d479bf9e6"
    def id2 = "0263a715-5fd4-4622-9b01-27daeeb9c357"

    def "should create new Car instance and return additional fetched attributes and _instanceName"() {
        when:
        def response = query(
                "datafetcher/upsert-car-return-instance-name.gql", asObjectNode("{\"id\":\"$id1\"}"))
        then:
        response.get('$.data.upsert_scr_Car._instanceName') == "TESLA - Z"
        response.get('$.data.upsert_scr_Car.garage') == null
        response.get('$.data.upsert_scr_Car.maxPassengers') == null
    }

    def "should create new Car instance and return additional fetched attributes without _instanceName"() {
        when:
        def response = query(
                "datafetcher/upsert-car.gql",
                asObjectNode('{"car": {' +
                        '"id": "' + id2 + '",' +
                        '"manufacturer":"TESLA",' +
                        '"model": "Z",' +
                        '"carType":"' + CarType.SEDAN + '"' +
                        '}}'),
        )
        then:
        response.get('$.data.upsert_scr_Car.garage') == null
        response.get('$.data.upsert_scr_Car.maxPassengers') == null
        response.get('$.data.upsert_scr_Car.purchaseDate') == null
    }

    def "should create datatypes test entity"() {
        when:
        def response = query(
                "datafetcher/upsert-datatypes-test-entity.graphql",
                asObjectNode("""{
                "entity":{
                    "dateAttr": "2021-01-01",
                    "dateTimeAttr":"2011-12-03T10:15:30",
                    "timeAttr":"10:33:12",
                    "localDateTimeAttr":"2021-06-06T12:31:00",
                    "localDateAttr":"2021-01-01",
                    "localTimeAttr":"10:33:12",
                    "offsetTimeAttr":"10:33:12+04:00",
                    "id":"6a538099-9dfd-8761-fa32-b496c236dbe9"
                }}"""))

        then:
        getBody(response) == '{"data":{"upsert_scr_DatatypesTestEntity":{' +
                '"id":"6a538099-9dfd-8761-fa32-b496c236dbe9",' +
                '"dateAttr":"2021-01-01",' +
                '"dateTimeAttr":"2011-12-03T10:15:30",' +
                '"timeAttr":"10:33:12",' +
                '"localDateTimeAttr":"2021-06-06T12:31:00",' +
                '"offsetDateTimeAttr":null,' +
                '"localDateAttr":"2021-01-01",' +
                '"localTimeAttr":"10:33:12",' +
                '"offsetTimeAttr":"10:33:12+04:00"' +
                '}}}'
    }

    @Ignore
    def "should create datatypes test entity with OffsetDateTime attribute"() {
        when:
        def response = query(
                "datafetcher/upsert-datatypes-test-entity.graphql",
                asObjectNode("""{
                "entity":{
                    "offsetDateTimeAttr": "2011-12-03T11:15:30+04:00",
                    "id":"6a538099-9dfd-8761-fa32-b496c236dbe9"
                }}"""))

        then:
        getBody(response) == '{"data":{"upsert_scr_DatatypesTestEntity":{' +
                '"id":"6a538099-9dfd-8761-fa32-b496c236dbe9",' +
                '"dateAttr":null,' +
                '"dateTimeAttr":null,' +
                '"timeAttr":null,' +
                '"localDateTimeAttr":null,' +
                '"offsetDateTimeAttr":"2011-12-03T11:15:30+04:00",' +
                '"localDateAttr":null,' +
                '"localTimeAttr":null,' +
                '"offsetTimeAttr":null' +
                '}}}'
    }
}
