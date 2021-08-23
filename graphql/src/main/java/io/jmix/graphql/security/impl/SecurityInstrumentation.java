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

package io.jmix.graphql.security.impl;

import graphql.ExecutionResult;
import graphql.execution.AbortExecutionException;
import graphql.execution.instrumentation.InstrumentationContext;
import graphql.execution.instrumentation.SimpleInstrumentation;
import graphql.execution.instrumentation.parameters.InstrumentationExecuteOperationParameters;
import graphql.language.Field;
import graphql.language.OperationDefinition;
import io.jmix.core.AccessManager;
import io.jmix.core.Messages;
import io.jmix.core.accesscontext.GraphQLOperationAccessContext;
import io.jmix.graphql.spqr.SpqrCustomSchemeRegistry;

public class SecurityInstrumentation extends SimpleInstrumentation {

    private final SpqrCustomSchemeRegistry schemeRegistry;
    private final AccessManager accessManager;
    private final Messages messages;

    public SecurityInstrumentation(SpqrCustomSchemeRegistry schemeRegistry, AccessManager accessManager, Messages messages) {
        this.schemeRegistry = schemeRegistry;
        this.accessManager = accessManager;
        this.messages = messages;
    }

    @Override
    public InstrumentationContext<ExecutionResult> beginExecuteOperation(InstrumentationExecuteOperationParameters parameters) {
        OperationDefinition operationDefinition = parameters.getExecutionContext().getOperationDefinition();

        if (operationDefinition.getOperation().equals(OperationDefinition.Operation.QUERY)
                || operationDefinition.getOperation().equals(OperationDefinition.Operation.MUTATION)) {

            String operationName = getOperationName(operationDefinition);

            GraphQLOperationAccessContext accessContext = new GraphQLOperationAccessContext(operationName);
            accessManager.applyRegisteredConstraints(accessContext);

            if (schemeRegistry.isCustomOperation(operationName) && !accessContext.isPermitted()) {
                throw new AbortExecutionException(messages.getMessage("io.jmix.graphql/gqlQueryAccessDenied"));
            }
        }
        return super.beginExecuteOperation(parameters);
    }

    private String getOperationName(OperationDefinition operationDefinition) {
        return ((Field) operationDefinition.getSelectionSet().getSelections().get(0)).getName();
    }
}
