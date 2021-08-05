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

import graphql.execution.AbortExecutionException;
import io.jmix.core.AccessManager;
import io.jmix.core.Messages;
import io.jmix.core.accesscontext.GraphQLOperationAccessContext;
import io.jmix.graphql.NamingUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.Arrays;

@Aspect
@Component("sec_graphQLSecurityAspect")
public class GraphQLSecurityAspect {
    @Autowired
    private AccessManager accessManager;
    @Autowired
    private Messages messages;

    public GraphQLSecurityAspect() {
    }

    @Pointcut("within(@io.leangen.graphql.spqr.spring.annotations.GraphQLApi *)")
    public void beanAnnotatedWithGraphQLApi() {
    }

    @Pointcut("execution(public * *(..))")
    public void publicMethod() {
    }

    @Pointcut("within(@io.jmix.graphql.security.GraphQLAnonymousAccess *)")
    public void anonymousAccessBean() {
    }

    @Pointcut("execution(@io.jmix.graphql.security.GraphQLAnonymousAccess * *(..))")
    public void anonymousAccessMethod() {
    }

    @Pointcut("!(anonymousAccessBean() || anonymousAccessMethod())")
    public void protectedQuery() {
    }

    @Around("beanAnnotatedWithGraphQLApi() && publicMethod() && protectedQuery()")
    public Object checkPermission(ProceedingJoinPoint joinPoint) throws Throwable {
        Method method = Arrays.stream(joinPoint.getTarget().getClass().getDeclaredMethods())
                .filter(m -> m.getName().equals(joinPoint.getSignature().getName()))
                .findFirst().orElse(null);

        GraphQLOperationAccessContext context = new GraphQLOperationAccessContext(NamingUtils.getQueryName(method));
        accessManager.applyRegisteredConstraints(context);
        if (!context.isPermitted()) {
            throw new AbortExecutionException(messages.getMessage("io.jmix.graphql/gqlQueryAccessDenied="));
        }
        return joinPoint.proceed();
    }
}
