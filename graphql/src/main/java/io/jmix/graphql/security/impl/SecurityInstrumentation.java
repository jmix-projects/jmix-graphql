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
import graphql.execution.instrumentation.parameters.InstrumentationExecutionParameters;
import io.jmix.core.AccessManager;
import io.jmix.core.Messages;
import io.jmix.core.accesscontext.GraphQLOperationAccessContext;
import io.jmix.graphql.spqr.SpqrCustomSchemeRegistry;
import org.springframework.beans.factory.annotation.Autowired;

public class SecurityInstrumentation extends SimpleInstrumentation {

    @Autowired
    private SpqrCustomSchemeRegistry schemeRegistry;
    @Autowired
    private AccessManager accessManager;
    @Autowired
    private Messages messages;

    @Override
    public InstrumentationContext<ExecutionResult> beginExecution(InstrumentationExecutionParameters parameters) {
        GraphQLOperationAccessContext accessContext = new GraphQLOperationAccessContext(parameters.getOperation());
        accessManager.applyRegisteredConstraints(accessContext);

        if (!schemeRegistry.isCustomOperation(parameters.getOperation()) || accessContext.isPermitted()) {
            return super.beginExecution(parameters);
        }
        throw new AbortExecutionException(messages.getMessage("io.jmix.graphql/gqlQueryAccessDenied"));
    }
}
