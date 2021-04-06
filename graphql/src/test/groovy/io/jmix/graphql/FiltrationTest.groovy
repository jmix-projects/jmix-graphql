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

package io.jmix.graphql

import graphql.Scalars
import graphql.language.*
import io.jmix.core.Metadata
import io.jmix.graphql.schema.FilterTypesBuilder
import io.jmix.graphql.schema.scalar.CustomScalars
import org.apache.commons.lang3.StringUtils
import org.springframework.beans.factory.annotation.Autowired
import test_support.entity.CarDto

import javax.annotation.Nullable

class FiltrationTest extends AbstractGraphQLTest {

    @Autowired
    private FilterTypesBuilder filterTypesBuilder
    @Autowired
    private Metadata metadata

    @SuppressWarnings('unused')
    void setup() {

    }

    def "buildFilterConditionType"() {
        given:
        def conditionType

        when:
        conditionType = filterTypesBuilder.buildFilterConditionType(metadata.findClass(CarDto))
        def standard = buildStandardFilterTypeCarDto()

        then:
        conditionType != null
        conditionType.isEqualTo(standard)
    }

    private static InputObjectTypeDefinition buildStandardFilterTypeCarDto() {
        String className = 'inp_scr_CarDtoFilterCondition'

        InputObjectTypeDefinition.Builder builder = InputObjectTypeDefinition.newInputObjectDefinition()
                .name(className)

        List<InputValueDefinition> valueDefs = new ArrayList<>()

        def suffix = 'FilterCondition'
        valueDefs.add(listValueDef("id", CustomScalars.GraphQLUUID.name + suffix, null))
        valueDefs.add(listValueDef("manufacturer", Scalars.GraphQLString.name + suffix, null))
        valueDefs.add(listValueDef("price", Scalars.GraphQLBigDecimal.name + suffix, null))
        valueDefs.add(listValueDef("model", Scalars.GraphQLString.name + suffix, null))
        valueDefs.add(listValueDef(FilterTypesBuilder.ConditionUnionType.AND.name(), className, null))
        valueDefs.add(listValueDef(FilterTypesBuilder.ConditionUnionType.OR.name(), className, null))

        builder.inputValueDefinitions(valueDefs)

        return builder.build()
    }

    private static InputValueDefinition listValueDef(String fieldName, String type, @Nullable String description) {
        return InputValueDefinition.newInputValueDefinition()
                .name(fieldName).type(new ListType(new TypeName(type)))
                .description(StringUtils.isBlank(description) ? null : new Description(description, null, false))
                .build()
    }
}
