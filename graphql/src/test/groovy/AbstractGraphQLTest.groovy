import graphql.Scalars
import graphql.language.InputObjectTypeDefinition
import graphql.language.InputValueDefinition
import io.jmix.core.CoreConfiguration
import io.jmix.core.MetadataTools
import io.jmix.core.metamodel.model.MetaClass
import io.jmix.core.metamodel.model.MetaProperty
import io.jmix.data.DataConfiguration
import io.jmix.eclipselink.EclipselinkConfiguration
import io.jmix.graphql.GraphqlConfiguration
import io.jmix.graphql.schema.FilterTypesBuilder
import io.jmix.graphql.schema.InpTypesBuilder
import io.jmix.graphql.schema.scalar.CustomScalars
import org.apache.commons.lang3.StringUtils
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.test.context.ContextConfiguration
import org.springframework.transaction.PlatformTransactionManager
import org.springframework.transaction.TransactionDefinition
import org.springframework.transaction.support.TransactionTemplate
import spock.lang.Specification
import test_support.GraphQLTestConfiguration
import test_support.TestContextInitializer

import java.time.LocalDateTime
import java.util.stream.Collectors

import static io.jmix.graphql.schema.Types.listValueDef

/*
 * Copyright 2019 Haulmont.
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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

@SuppressWarnings('SpringJavaInjectionPointsAutowiringInspection')
@ContextConfiguration(
        classes = [CoreConfiguration, DataConfiguration, EclipselinkConfiguration,
                GraphQLTestConfiguration, GraphqlConfiguration],
        initializers = [TestContextInitializer]
)
class AbstractGraphQLTest extends Specification {

    @Autowired
    protected JdbcTemplate jdbc
    @Autowired
    private InpTypesBuilder inpTypesBuilder
    @Autowired
    private MetadataTools metadataTools

    protected TransactionTemplate transaction

    @Autowired
    protected void setTransactionManager(PlatformTransactionManager transactionManager) {
        transaction = new TransactionTemplate(transactionManager)
        transaction.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW)
    }

    void clearTables(String... tableNames) {
        tableNames.each {
            clearTable(it)
        }
    }

    protected void clearTable(String tableName) {
        //runSqlUpdate("CREATE TABLE IF NOT EXISTS $tableName")
        runSqlUpdate("delete from $tableName")
    }

    protected void runSqlUpdate(String sqlUpdateString) {
        transaction.executeWithoutResult({ status ->
            jdbc.update(sqlUpdateString)
        })
    }

    protected InputObjectTypeDefinition buildStandardFilterType(MetaClass metaClass) {
        String className = composeFilterConditionTypeName(metaClass)

        InputObjectTypeDefinition.Builder builder = InputObjectTypeDefinition.newInputObjectDefinition()
                .name(className)

        List<InputValueDefinition> valueDefs = metaClass.getProperties().stream()
                .map({ metaProperty ->

                    if (metaProperty.getType() == MetaProperty.Type.ENUM) {
                        return listValueDef(metaProperty.getName(), "String", null)
                    }

                    if (metaProperty.getRange().getCardinality().isMany()) {
                        return null
                    }

                    String typeName = composeFilterConditionTypeName(getFieldTypeName(metaProperty))
                    return listValueDef(metaProperty.getName(), typeName, null)
                })
                .filter(Objects.&nonNull)
                .collect(Collectors.toList())

        valueDefs.add(listValueDef(FilterTypesBuilder.ConditionUnionType.AND.name(), className, null))
        valueDefs.add(listValueDef(FilterTypesBuilder.ConditionUnionType.OR.name(), className, null))

        builder.inputValueDefinitions(valueDefs)

        return builder.build()
    }

    protected static String composeFilterConditionTypeName(MetaClass metaClass) {
        return composeFilterConditionTypeName(metaClass.getName())
    }

    protected static String composeFilterConditionTypeName(String metaClass) {
        return composeFilterTypeName(metaClass, "FilterCondition")
    }

    protected static String composeFilterTypeName(String name, String suffix) {
        if (!name.startsWith("inp_")) {
            name = normalizeName(name)
        }
        return name + suffix
    }

    protected static String normalizeName(String entityName) {
        return "inp_" + StringUtils.uncapitalize(entityName.replaceAll('\\$', '_'))
    }

    protected String getFieldTypeName(MetaProperty metaProperty) {
        if (metaProperty.getType() == MetaProperty.Type.DATATYPE) {
            return getDatatypeFieldTypeName(metaProperty)
        }

        if (metaProperty.getType() == MetaProperty.Type.ENUM) {
            return getEnumFieldTypeName(metaProperty.getJavaType())
        }

        if ((metaProperty.getType() == MetaProperty.Type.ASSOCIATION || metaProperty.getType() == MetaProperty.Type.COMPOSITION)) {
            return getReferenceTypeName(metaProperty)
        }

        throw new UnsupportedOperationException(String.format("Can't define field type name for metaProperty %s class %s", metaProperty, metaProperty.getJavaType()))
    }

    protected String getReferenceTypeName(MetaProperty metaProperty) {
        if (metadataTools.isJpaEntity(metaProperty.getRange().asClass())) {
            return normalizeName(metaProperty.getRange().asClass().getName())
        } else {
            return "String"
        }
    }
    protected static String getEnumFieldTypeName(Class<?> javaType) {
        return javaType.getSimpleName()
    }
    protected static String getDatatypeFieldTypeName(MetaProperty metaProperty) {
        Class<?> javaType = metaProperty.getRange().asDatatype().getJavaClass()

        if (String.class.isAssignableFrom(javaType))
            return Scalars.GraphQLString.getName()
        if (Integer.class.isAssignableFrom(javaType) || int.class.isAssignableFrom(javaType)) {
            return Scalars.GraphQLInt.getName()
        }
        if (Short.class.isAssignableFrom(javaType) || short.class.isAssignableFrom(javaType)) {
            return Scalars.GraphQLShort.getName()
        }
        if (Float.class.isAssignableFrom(javaType) || float.class.isAssignableFrom(javaType)
                || Double.class.isAssignableFrom(javaType) || double.class.isAssignableFrom(javaType)) {
            return Scalars.GraphQLFloat.getName()
        }
        if (Boolean.class.isAssignableFrom(javaType) || boolean.class.isAssignableFrom(javaType)) {
            return Scalars.GraphQLBoolean.getName()
        }

        if (UUID.class.isAssignableFrom(javaType)) {
            return CustomScalars.GraphQLUUID.getName()
        }
        if (Long.class.isAssignableFrom(javaType) || long.class.isAssignableFrom(javaType)) {
            return CustomScalars.GraphQLLong.getName()
        }
        if (BigDecimal.class.isAssignableFrom(javaType)) {
            return CustomScalars.GraphQLBigDecimal.getName()
        }
        if (Date.class.isAssignableFrom(javaType)) {
            return CustomScalars.GraphQLDate.getName()
        }
        if (LocalDateTime.class.isAssignableFrom(javaType)) {
            return CustomScalars.GraphQLLocalDateTime.getName()
        }

        return "String"
    }
}
