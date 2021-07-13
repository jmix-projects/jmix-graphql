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

package io.jmix.graphql.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import graphql.GraphQL;
import groovy.util.MapEntry;
import io.leangen.graphql.spqr.spring.web.GraphQLController;
import io.leangen.graphql.spqr.spring.web.dto.GraphQLRequest;
import io.leangen.graphql.spqr.spring.web.mvc.GraphQLMvcExecutor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import javax.servlet.ServletException;
import javax.servlet.http.Part;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;

@RestController
@CrossOrigin
public class FilesController extends GraphQLController<NativeWebRequest> {

    @Autowired
    public FilesController(GraphQL graphQL, GraphQLMvcExecutor executor) {
        super(graphQL, executor);
    }

    /**
     * For Requests that follow the GraphQL Multipart Request Spec from: https://github.com/jaydenseric/graphql-multipart-request-spec
     *
     * The Request contains the following parts:
     * operations: JSON String with the GQL Query
     * map: Maps the multipart files to the variables of the GQL Query
     */
    @PostMapping(
            value = "${graphql.spqr.http.endpoint:/graphql}",
            consumes = {MediaType.MULTIPART_FORM_DATA_VALUE},
            produces = MediaType.APPLICATION_JSON_VALUE
    )
    @ResponseBody
    public Object executeMultipartPost(@RequestPart("operations") String operations,
                                       @RequestPart("map") String map,
                                       MultipartHttpServletRequest multiPartRequest,
                                       NativeWebRequest webRequest) throws IOException, ServletException {
        GraphQLRequest graphQLRequest = new ObjectMapper().readerFor(GraphQLRequest.class).readValue(operations);
        Map<String, ArrayList<String>> fileMap = new ObjectMapper().readerFor(Map.class).readValue(map);

        mapRequestFilesToVariables(multiPartRequest, graphQLRequest, fileMap);
        return this.executeJsonPost(graphQLRequest, new GraphQLRequest(null, null, null, null), webRequest);
    }

    /**
     * Maps the files that were sent in a Multipart Request to the corresponding variables of a {@link GraphQLRequest}.
     * This makes it possible to use a file input like a normal parameter in a GraphQLApi Method.
     */
    private void mapRequestFilesToVariables(MultipartHttpServletRequest multiPartRequest, GraphQLRequest graphQLRequest,
                                            Map<String, ArrayList<String>> fileMap) throws IOException, ServletException {
        for (Map.Entry<String, ArrayList<String>> pair : fileMap.entrySet()) {
            String targetVariable = pair.getValue().get(0).replace("variables.", "");
            if(graphQLRequest.getVariables().containsKey(targetVariable)) {
                Part correspondingFile = multiPartRequest.getPart(pair.getKey());
                graphQLRequest.getVariables().put(targetVariable, correspondingFile);
            }
        }
    }
}
