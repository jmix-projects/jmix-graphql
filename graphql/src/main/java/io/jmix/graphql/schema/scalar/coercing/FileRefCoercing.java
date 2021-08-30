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

package io.jmix.graphql.schema.scalar.coercing;

import graphql.schema.CoercingParseValueException;
import graphql.schema.CoercingSerializeException;
import io.jmix.core.FileRef;
import io.jmix.core.FileStorage;
import io.jmix.core.metamodel.datatype.impl.FileRefDatatype;
import io.jmix.graphql.service.FileService;
import org.springframework.web.multipart.MultipartFile;

import java.io.FileNotFoundException;
import java.util.AbstractMap;

public class FileRefCoercing extends BaseScalarCoercing {

    @Override
    public Object serialize(Object o) throws CoercingSerializeException {
        return ((FileRef) o).toString();
    }

    @Override
    public Object parseValue(Object o) throws CoercingParseValueException {
        String storageName = null;
        FileService fileService = FileService.get();
        try {
            if (o instanceof AbstractMap.SimpleEntry) {

                    storageName = ((AbstractMap.SimpleEntry<String, MultipartFile>) o).getKey();
                    FileStorage fileStorage = fileService.getFileStorage(storageName);
                    MultipartFile multipartFile = ((AbstractMap.SimpleEntry<String, MultipartFile>) o).getValue();

                    FileRefDatatype refDatatype = new FileRefDatatype();
                    FileRef fileRef = fileService.saveFileIntoStorage(multipartFile, fileStorage);
                    return refDatatype.format(fileRef);

            }
            if (o instanceof String) {

                    FileRef fileRef = FileRef.fromString((String) o);
                    FileStorage fileStorage = fileService.getFileStorage(fileRef.getStorageName());
                    if (!fileStorage.fileExists(fileRef)) {
                        throw new FileNotFoundException();
                    }
                    return o;
            }
        }
        catch (Exception e) {
            throw new CoercingParseValueException("File storage with name " + storageName + " not found");
        }
        return null;
    }

}
