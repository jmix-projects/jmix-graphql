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
import com.google.common.base.Strings;
import graphql.GraphQL;
import io.jmix.core.AccessManager;
import io.jmix.core.FileRef;
import io.jmix.core.FileStorage;
import io.jmix.core.FileStorageException;
import io.jmix.core.FileStorageLocator;
import io.jmix.core.Metadata;
import io.jmix.core.common.util.URLEncodeUtils;
import io.jmix.graphql.service.FilePermissionService;
import io.leangen.graphql.spqr.spring.web.GraphQLController;
import io.leangen.graphql.spqr.spring.web.dto.GraphQLRequest;
import io.leangen.graphql.spqr.spring.web.mvc.GraphQLMvcExecutor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;
import org.springframework.web.util.UriComponents;
import org.springframework.web.util.UriComponentsBuilder;

import javax.annotation.Nullable;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import java.io.InputStream;
import java.net.URI;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;

@RestController("graphql_FilesController")
@CrossOrigin
public class GraphQLFilesUploadController extends GraphQLController<NativeWebRequest> {

    @Autowired
    public GraphQLFilesUploadController(GraphQL graphQL, GraphQLMvcExecutor executor) {
        super(graphQL, executor);
    }

    @Autowired
    protected FilePermissionService filePermissionService;

    private static final Logger log = LoggerFactory.getLogger(GraphQLFilesUploadController.class);

    /**
     * For Requests that follow the GraphQL Multipart Request Spec from: https://github.com/jaydenseric/graphql-multipart-request-spec
     * <p>
     * The Request contains the following parts:
     * operations: JSON String with the GQL Query
     * map: Maps the multipart files to the variables of the GQL Query
     */
    @PostMapping(
            value = "${graphql.spqr.http.endpoint:/graphql}",
            consumes = {MediaType.ALL_VALUE}
    )
    @ResponseBody
    public Object executeMultipartPost(@RequestPart("operations") String operations,
                                       @RequestPart("map") String map,
                                       @RequestPart(value = "storageName", required = false) String storageName,
                                       MultipartHttpServletRequest multiPartRequest,
                                       NativeWebRequest webRequest) throws Exception {
        filePermissionService.checkFileUploadPermission();

        GraphQLRequest graphQLRequest = new ObjectMapper().readerFor(GraphQLRequest.class).readValue(operations);
        Map<String, ArrayList<String>> fileMap = new ObjectMapper().readerFor(Map.class).readValue(map);

        mapRequestFilesToVariables(multiPartRequest, graphQLRequest, fileMap, storageName);
        return this.executeJsonPost(graphQLRequest, new GraphQLRequest(null, null, null, null), webRequest);
    }

    /**
     * Maps the files that were sent in a Multipart Request to the corresponding variables of a {@link GraphQLRequest}.
     * This makes it possible to use a file input like a normal parameter in a GraphQLApi Method.
     */
    private void mapRequestFilesToVariables(MultipartHttpServletRequest multiPartRequest, GraphQLRequest graphQLRequest,
                                            Map<String, ArrayList<String>> fileMap, String storage) {
        for (Map.Entry<String, ArrayList<String>> pair : fileMap.entrySet()) {
            String targetVariable = pair.getValue().get(0).replace("variables.", "");
            if (graphQLRequest.getVariables().containsKey(targetVariable)) {
                MultipartFile correspondingFile = multiPartRequest.getFileMap().get(pair.getKey());
                if (correspondingFile == null) {
                    throw new GraphQLControllerException("File upload failed",  "Field with name " + pair.getKey() + " of multipart request is absent",
                            HttpStatus.BAD_REQUEST);
                }
                graphQLRequest.getVariables().put(targetVariable,
                        new AbstractMap.SimpleEntry<>(storage, correspondingFile));
            }
        }
    }

    @Autowired
    protected Metadata metadata;

    @Autowired
    protected FileStorageLocator fileStorageLocator;

    @Autowired
    protected AccessManager accessManager;

    /**
     * Method for simple file upload. File contents are placed in the request body. Optional file name parameter is
     * passed as a query param.
     */
    @PostMapping(path = "${graphql.spqr.http.endpoint:/graphql/files}", consumes = "!multipart/form-data")
    public ResponseEntity<FileInfo> uploadFile(HttpServletRequest request,
                                               @RequestParam(required = false) String name,
                                               @RequestParam(required = false) String storageName) {
        filePermissionService.checkFileUploadPermission();

        FileStorage fileStorage = getFileStorage(storageName);
        try {
            String contentLength = request.getHeader("Content-Length");

            long size = 0;
            try {
                size = Long.parseLong(contentLength);
            } catch (NumberFormatException ignored) {
            }

            ServletInputStream is = request.getInputStream();
            name = Objects.toString(name, "");
            FileRef fileRef = uploadToFileStorage(fileStorage, is, name);

            return createFileInfoResponseEntity(request, fileRef, name, size);
        } catch (Exception e) {
            log.error("File upload failed", e);
            throw new GraphQLControllerException("File upload failed", "File upload failed", HttpStatus.INTERNAL_SERVER_ERROR, e);
        }
    }

    /**
     * Method for multipart file upload. It expects the file contents to be passed in the part called 'file'.
     */
    @PostMapping(path = "${graphql.spqr.http.endpoint:/graphql/files}", consumes = "multipart/form-data")
    public ResponseEntity<FileInfo> uploadFile(@RequestParam("file") MultipartFile file,
                                               @RequestParam(required = false) String name,
                                               @RequestParam(required = false) String storageName,
                                               HttpServletRequest request) {
        filePermissionService.checkFileUploadPermission();

        FileStorage fileStorage = getFileStorage(storageName);
        try {
            if (Strings.isNullOrEmpty(name)) {
                name = file.getOriginalFilename();
            }
            name = Objects.toString(name, "");

            long size = file.getSize();

            InputStream is = file.getInputStream();
            FileRef fileRef = uploadToFileStorage(fileStorage, is, name);

            return createFileInfoResponseEntity(request, fileRef, name, size);
        } catch (Exception e) {
            log.error("File upload failed", e);
            throw new GraphQLControllerException("File upload failed", "File upload failed", HttpStatus.INTERNAL_SERVER_ERROR, e);
        }
    }

    protected FileStorage getFileStorage(@Nullable String storageName) {
        if (Strings.isNullOrEmpty(storageName)) {
            return fileStorageLocator.getDefault();
        } else {
            try {
                return fileStorageLocator.getByName(storageName);
            } catch (IllegalArgumentException e) {
                throw new GraphQLControllerException("Invalid storage name",
                        String.format("Cannot find FileStorage with the given name: '%s'", storageName),
                        HttpStatus.BAD_REQUEST,
                        e);
            }
        }
    }

    protected ResponseEntity<FileInfo> createFileInfoResponseEntity(HttpServletRequest request,
                                                                    FileRef fileRef, String filename, long size) {
        FileInfo fileInfo = new FileInfo(fileRef.toString(), filename, size);

        UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(request.getRequestURL().toString())
                .queryParam("fileRef", URLEncodeUtils.encodeUtf8(fileRef.toString()))
                .buildAndExpand();

        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setLocation(URI.create(uriComponents.toUriString()));
        return new ResponseEntity<>(fileInfo, httpHeaders, HttpStatus.CREATED);
    }

    protected FileRef uploadToFileStorage(FileStorage fileStorage, InputStream is, String fileName)
            throws FileStorageException {
        try {
            return fileStorage.saveStream(fileName, is);
        } catch (FileStorageException e) {
            throw new GraphQLControllerException("Unable to upload file to FileStorage",
                    "Unable to upload file to FileStorage: " + fileName,
                    HttpStatus.INTERNAL_SERVER_ERROR,
                    e);
        }
    }

    public class FileInfo {
        protected String fileRef;
        protected String name;
        protected long size;

        public FileInfo(String fileRef, String name, long size) {
            this.fileRef = fileRef;
            this.name = name;
            this.size = size;
        }

        public String getFileRef() {
            return fileRef;
        }

        public void setFileRef(String fileRef) {
            this.fileRef = fileRef;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public long getSize() {
            return size;
        }

        public void setSize(long size) {
            this.size = size;
        }
    }
}
