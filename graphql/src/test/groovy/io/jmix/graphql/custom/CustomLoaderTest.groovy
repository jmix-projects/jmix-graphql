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
import org.springframework.test.context.ActiveProfiles
import spock.lang.Ignore

@Ignore
@ActiveProfiles("custom")
class CustomLoaderTest extends AbstractGraphQLTest {

    def "Custom cars loader "() {
        when:
        def response = query(
                "datafetcher/query-cars.gql"
        )

        then:
        def body = getBody(response)
        body == '{"data":{"scr_CarList":[' +
                '{"_instanceName":"BMW - M3","price":"10"},' +
                '{"_instanceName":"Lada - Vesta","price":"20"}' +
                ']}}'
    }

    def "Custom car count loader"() {
        when:
        def response = query(
                "datafetcher/query-car-count.gql"
        )
        then:
        getBody(response) == '{"data":{"scr_CarCount":"999"}}'
    }

    def "Custom car loader"() {
        when:
        def response = query(
                "datafetcher/query-car.gql",
                asObjectNode('{"id": "123e4567-e89b-12d3-a456-426655440000"}')
        )
        then:
        def body = getBody(response)
        body == '{"data":{"scr_CarById":' +
                '{"_instanceName":"Lada - Vesta","price":"10"}' +
                '}}'
    }
//    @LocalServerPort
//    int port
//
//    @Autowired
//    GraphQLTestTemplate graphQLTestTemplate
//    @Autowired
//    ResourceRoleRepository resourceRoleRepository
//    @Autowired
//    InMemoryUserRepository userRepository
//
//    protected TransactionTemplate transaction
//    protected String adminToken
//    protected String mechanicToken
//    protected UserDetails admin
//    protected UserDetails mechanic
//
//    void setup() {
//        admin = User.builder()
//                .username("admin")
//                .password("{noop}admin")
//                .authorities(RoleGrantedAuthority.ofResourceRole(resourceRoleRepository.getRoleByCode("system-full-access")))
//                .build()
//        userRepository.addUser(admin)
//
//        mechanic = User.builder()
//                .username("mechanic")
//                .password("{noop}1")
//                .authorities(RoleGrantedAuthority.ofResourceRole(resourceRoleRepository.getRoleByCode("mechanics")))
//                .build()
//        userRepository.addUser(mechanic)
//
//        adminToken = RestTestUtils.getAuthToken("admin", "admin", port)
//        mechanicToken = RestTestUtils.getAuthToken("mechanic", "1", port)
//    }
//
//    @Autowired
//    protected void setTransactionManager(PlatformTransactionManager transactionManager) {
//        transaction = new TransactionTemplate(transactionManager)
//        transaction.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW)
//    }
//
//    protected query(String queryFilePath) {
//        return graphQLTestTemplate
//                .withBearerAuth(adminToken)
//                .postForResource("graphql/io/jmix/graphql/" + queryFilePath)
//    }
//
//    protected query(String queryFilePath, HttpHeaders httpHeaders) {
//        return graphQLTestTemplate
//                .withHeaders(httpHeaders)
//                .withBearerAuth(adminToken)
//                .postForResource("graphql/io/jmix/graphql/" + queryFilePath)
//    }
//
//    protected query(String queryFilePath, ObjectNode variables) {
//        return query(queryFilePath, variables, adminToken)
//    }
//
//    protected query(String queryFilePath, ObjectNode variables, String token) {
//        return graphQLTestTemplate
//                .withBearerAuth(token)
//                .perform("graphql/io/jmix/graphql/" + queryFilePath, variables)
//    }
//
//    static ObjectNode asObjectNode(String str) {
//        return new ObjectMapper().readValue(str, ObjectNode.class)
//    }
//
//    static String getBody(GraphQLResponse response) {
//        return response.rawResponse.body
//    }
//
//    static JsonObject getExtensions(JsonObject error) {
//        error.getAsJsonObject("extensions").getAsJsonObject()
//    }
//
//    static String getMessage(JsonObject jsonObject) {
//        jsonObject.get("message").getAsString()
//    }
//
//    static String getPath(JsonObject jsonObject) {
//        jsonObject.get("path").getAsString()
//    }
//
//    static JsonArray getErrors(GraphQLResponse response) {
//        JsonParser.parseString(response.rawResponse.body).getAsJsonArray("errors")
//    }
}
