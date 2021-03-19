import io.jmix.core.Metadata
import io.jmix.graphql.schema.FilterTypesBuilder
import org.springframework.beans.factory.annotation.Autowired
import test_support.entity.CarDto

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
        def standard = buildStandardFilterType(metadata.findClass(CarDto))

        then:
        conditionType != null
        conditionType.isEqualTo(standard)
    }
}
