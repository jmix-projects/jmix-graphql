import graphql.language.*
import io.jmix.core.Metadata
import io.jmix.graphql.schema.FilterManager
import io.jmix.graphql.schema.FilterTypesBuilder
import io.jmix.graphql.schema.Types
import io.jmix.graphql.schema.scalar.CustomScalars
import org.apache.commons.lang3.StringUtils
import org.springframework.beans.factory.annotation.Autowired
import test_support.entity.CarDto

import javax.annotation.Nullable

import static graphql.Scalars.*
import static io.jmix.graphql.schema.Types.FilterOperation.*
import static io.jmix.graphql.schema.scalar.CustomScalars.GraphQLUUID
import static io.jmix.graphql.schema.scalar.CustomScalars.GraphQLVoid

class FiltrationTest extends AbstractGraphQLTest {

    @Autowired
    private FilterTypesBuilder filterTypesBuilder
    @Autowired
    private Metadata metadata
    @Autowired
    private FilterManager filterManager
    private EnumSet<Types.FilterOperation> stringExpectation

    @SuppressWarnings('unused')
    void setup() {
        stringExpectation = EnumSet.of(EQ, NEQ, IN_LIST, NOT_IN_LIST, STARTS_WITH, ENDS_WITH, CONTAINS, NOT_EMPTY, IS_NULL)
    }

    def "buildScalarFilterConditionType for GraphQLString"() {
        given:
        def builder = InputObjectTypeDefinition.newInputObjectDefinition()
                .name("inp_stringFilterCondition")
        for (Types.FilterOperation operation : stringExpectation) {
            if (IN_LIST == operation || NOT_IN_LIST == operation) {
                builder.inputValueDefinition(listValueDef(operation, GraphQLString.name))
            } else {
                builder.inputValueDefinition(valueDef(operation, GraphQLString.name))
            }
        }
        def expectation = builder.build()

        when:
        def actual = filterTypesBuilder.buildScalarFilterConditionType(GraphQLString)

        then:
        actual.isEqualTo(expectation)
    }

    @SuppressWarnings('UnnecessaryQualifiedReference')
    def "available operations for scalars"() {
        given:
        def uuidExpectation = EnumSet.of(EQ, NEQ, IN_LIST, NOT_IN_LIST, NOT_EMPTY, IS_NULL)
        def numbersExpectation = EnumSet.of(EQ, NEQ, GT, GTE, LT, LTE, IN_LIST, NOT_IN_LIST, NOT_EMPTY, IS_NULL)
        def dateTimeExpectation = EnumSet.of(EQ, NEQ, IN_LIST, NOT_IN_LIST, GT, GTE, LT, LTE, NOT_EMPTY, IS_NULL)
        def booleanExpectation = EnumSet.of(EQ, NEQ, NOT_EMPTY, IS_NULL)


        when:
        def uuidActual = filterManager.availableOperations(CustomScalars.GraphQLUUID)
        def booleanActual = filterManager.availableOperations(GraphQLBoolean)

        //numberTypes
        def bigDecimalActual = filterManager.availableOperations(CustomScalars.GraphQLBigDecimal)
        def longActual = filterManager.availableOperations(CustomScalars.GraphQLLong)
        def byteActual = filterManager.availableOperations(GraphQLByte)
        def shortActual = filterManager.availableOperations(GraphQLShort)
        def floatActual = filterManager.availableOperations(GraphQLFloat)
        def bigIntActual = filterManager.availableOperations(GraphQLBigInteger)
        def intActual = filterManager.availableOperations(GraphQLInt)

        //dateTimeTypes
        def localDateTimeActual = filterManager.availableOperations(CustomScalars.GraphQLLocalDateTime)
        def dateActual = filterManager.availableOperations(CustomScalars.GraphQLDate)

        //stringTypes
        def stringActual = filterManager.availableOperations(GraphQLString)
        def charActual = filterManager.availableOperations(GraphQLChar)

        then:
        uuidActual == uuidExpectation
        booleanActual == booleanExpectation
        bigDecimalActual == numbersExpectation
        longActual == numbersExpectation
        byteActual == numbersExpectation
        shortActual == numbersExpectation
        floatActual == numbersExpectation
        bigIntActual == numbersExpectation
        intActual == numbersExpectation
        localDateTimeActual == dateTimeExpectation
        dateActual == dateTimeExpectation
        stringActual == this.stringExpectation
        charActual == this.stringExpectation
    }

    def "doesn't support operation"() {
        given:
        def unsupportedScalar = GraphQLVoid

        when:
        filterManager.availableOperations(unsupportedScalar)

        then:
        thrown UnsupportedOperationException
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
        valueDefs.add(listValueDef("id", GraphQLUUID.name + suffix, null))
        valueDefs.add(listValueDef("manufacturer", GraphQLString.name + suffix, null))
        valueDefs.add(listValueDef("price", GraphQLBigDecimal.name + suffix, null))
        valueDefs.add(listValueDef("model", GraphQLString.name + suffix, null))
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

    private static InputValueDefinition valueDef(Types.FilterOperation operation, String type) {
        return valueDef(operation.getId(), type, operation.description)
    }

    private static InputValueDefinition valueDef(String fieldName, String type, @Nullable String description) {
        return InputValueDefinition.newInputValueDefinition()
                .name(fieldName).type(new TypeName(type))
                .description(StringUtils.isBlank(description) ? null : new Description(description, null, false))
                .build()
    }

    private static InputValueDefinition listValueDef(Types.FilterOperation operation, String type) {
        return listValueDef(operation.getId(), type, operation.getDescription())
    }
}
