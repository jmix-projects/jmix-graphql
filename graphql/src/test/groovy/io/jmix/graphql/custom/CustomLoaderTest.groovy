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

package io.jmix.graphql.custom

import io.jmix.graphql.AbstractGraphQLTest

class CustomLoaderTest extends AbstractGraphQLTest{
    def "filter conditions union by AND"() {
        when:
        def response = query(
                "datafetcher/query-cars.gql",
                asObjectNode('{"filter": {"AND": [' +
                        '  {"price": {"_lte": "30"}},' +
                        '  {"price": {"_isNull": false}}' +
                        ']}}')
        )

        then:
        def body = getBody(response)
        body == '{"data":{"scr_CarList":[' +
                '{"_instanceName":"GAZ - 2410","price":"10"},' +
                '{"_instanceName":"Tesla - Model Y","price":"30"},' +
                '{"_instanceName":"Audi - 2141","price":"20"}' +
                ']}}'
    }
}
