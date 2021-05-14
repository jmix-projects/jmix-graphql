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

package io.jmix.graphql.schema.scalars

import graphql.language.StringValue
import graphql.schema.Coercing
import graphql.schema.CoercingParseLiteralException
import graphql.schema.CoercingSerializeException
import io.jmix.graphql.schema.scalar.LocalTimeScalar
import spock.lang.Specification

import java.time.LocalTime
import java.time.format.DateTimeFormatter

class LocalTimeScalarTest extends Specification {

    private final LocalTimeScalar scalar = new LocalTimeScalar()
    private Coercing coercing

    @SuppressWarnings('unused')
    def setup() {
        coercing = scalar.getCoercing()
    }

    def "localTime scalar test"() {
        given:
        def stringDate = new StringValue("23:59:59")
        def localTime = LocalTime.from(
                DateTimeFormatter
                        .ISO_LOCAL_TIME
                        .parse(stringDate.getValue())
        )
        def parsedLiteral
        def parsedValue
        def serialized
        def nullParsedLiteral
        def nullParsedValue

        when:
        parsedLiteral = (LocalTime) coercing.parseLiteral(stringDate)
        parsedValue = (LocalTime) coercing.parseValue(stringDate.getValue())
        serialized = coercing.serialize(localTime)
        nullParsedLiteral = (LocalTime) coercing.parseLiteral(new StringValue(""))
        nullParsedValue = (LocalTime) coercing.parseValue("")

        then:
        parsedLiteral == localTime
        parsedValue == localTime
        serialized == stringDate.getValue()
        nullParsedLiteral == LocalTime.MIN
        nullParsedValue == LocalTime.MIN
    }

    def "localTime scalar throws CoercingSerializeException"() {
        when:
        coercing.serialize("")

        then:
        def exception = thrown(CoercingSerializeException)
        exception.message == "Expected type 'LocalTime' but was 'String'."
    }

    def "localTime scalar throws CoercingParseLiteralException with parseLiteral"() {
        when:
        coercing.parseLiteral("")

        then:
        def exception = thrown(CoercingParseLiteralException)
        exception.message == "Expected type 'StringValue' but was 'String'."
    }

    def "localTime scalar throws CoercingParseLiteralException with parseValue"() {
        when:
        coercing.parseValue(new StringValue(""))

        then:
        def exception = thrown(CoercingParseLiteralException)
        exception.message == "Expected type 'String' but was 'StringValue'."
    }
}
