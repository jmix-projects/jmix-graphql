package io.jmix.graphql.datafetcher;

import graphql.schema.DataFetcher;
import io.jmix.core.*;
import io.jmix.core.accesscontext.CrudEntityContext;
import io.jmix.core.metamodel.model.MetaClass;
import io.jmix.core.querycondition.Condition;
import io.jmix.core.querycondition.LogicalCondition;
import io.jmix.graphql.NamingUtils;
import io.jmix.graphql.annotation.GraphqlLoader;
import io.jmix.graphql.loader.GraphqlEntityCountLoader;
import io.jmix.graphql.loader.GraphqlEntityListLoader;
import io.jmix.graphql.loader.GraphqlEntityLoader;
import io.jmix.graphql.schema.Types;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.Nullable;
import javax.annotation.PostConstruct;
import javax.validation.constraints.NotNull;
import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

@Component
public class EntityQueryDataFetcher {

    public static final int DEFAULT_MAX_RESULTS = 100;

    private final Logger log = LoggerFactory.getLogger(EntityQueryDataFetcher.class);

    private Map<Class<?>, Object> countLoaders;

    private Map<Class<?>, Object> entityLoaders;

    private Map<Class<?>, Object> entitiesLoaders;

    private static final String GRAPHQL_ENTITY_LOADER_METHOD_NAME = GraphqlEntityLoader.class.getDeclaredMethods()[0].getName();

    private static final String GRAPHQL_ENTITIES_LOADER_METHOD_NAME = GraphqlEntityListLoader.class.getDeclaredMethods()[0].getName();

    private static final String GRAPHQL_COUNT_LOADER_METHOD_NAME = GraphqlEntityCountLoader.class.getDeclaredMethods()[0].getName();

    @Autowired
    ResponseBuilder responseBuilder;
    @Autowired
    protected DataManager dataManager;
    @Autowired
    protected DataFetcherPlanBuilder dataFetcherPlanBuilder;
    @Autowired
    protected FilterConditionBuilder filterConditionBuilder;
    @Autowired
    protected AccessManager accessManager;
    @Autowired
    protected EnvironmentUtils environmentUtils;
    @Autowired
    protected MetadataTools metadataTools;
    @Autowired
    private ListableBeanFactory listableBeanFactory;

    public DataFetcher<?> loadEntity(MetaClass metaClass) {
        return environment -> {

            checkCanReadEntity(metaClass);

            String id = environment.getArgument("id");
            LoadContext<?> lc = new LoadContext<>(metaClass);
            // todo support not only UUID types of id
            lc.setId(UUID.fromString(id));
            FetchPlan fetchPlan = dataFetcherPlanBuilder.buildFetchPlan(metaClass.getJavaClass(), environment);
            lc.setFetchPlan(fetchPlan);

            log.debug("loadEntity: with context {}", lc);
            if(!entityLoaders.containsKey(metaClass.getClass())) {
                Object entity = dataManager.load(lc);
                if (entity == null) return null;

                return responseBuilder.buildResponse((Entity) entity, fetchPlan, metaClass, environmentUtils.getDotDelimitedProps(environment));
            } else {
                Object bean = entityLoaders.get(metaClass.getClass());
                Method method = bean.getClass().getDeclaredMethod(GRAPHQL_ENTITY_LOADER_METHOD_NAME, LoadContext.class);
                return responseBuilder.buildResponse((Entity) method.invoke(bean, lc), fetchPlan, metaClass,
                        environmentUtils.getDotDelimitedProps(environment));
            }
        };
    }

    public DataFetcher<List<Map<String, Object>>> loadEntities(MetaClass metaClass) {
        return environment -> {

            checkCanReadEntity(metaClass);

            Object filter = environment.getArgument(NamingUtils.FILTER);
            Object orderBy = environment.getArgument(NamingUtils.ORDER_BY);
            Integer limit = environment.getArgument(NamingUtils.LIMIT);
            Integer offset = environment.getArgument(NamingUtils.OFFSET);
            log.debug("loadEntities: metClass:{}, filter:{}, limit:{}, offset:{}, orderBy: {}",
                    metaClass, filter, limit, offset, orderBy);

            // fetch plan
            FetchPlan fetchPan = dataFetcherPlanBuilder.buildFetchPlan(metaClass.getJavaClass(), environment);

            // build filter condition
            LogicalCondition condition = createCondition(filter);
            LoadContext.Query query = generateQuery(metaClass, condition);
            query.setMaxResults(limit != null ? limit : DEFAULT_MAX_RESULTS);
            query.setFirstResult(offset == null ? 0 : offset);

            // orderBy and sort
            Pair<String, Types.SortOrder> orderByPathAndOrder = buildOrderBy("", orderBy);
            if (orderByPathAndOrder != null) {
                String path = orderByPathAndOrder.getKey();
                Types.SortOrder sortOrder = orderByPathAndOrder.getValue();
                query.setSort(Sort.by(
                        sortOrder == Types.SortOrder.ASC ? Sort.Order.asc(path) : Sort.Order.desc(path)));
            } else {
                String lastModifiedDateProperty = metadataTools.findLastModifiedDateProperty(metaClass.getJavaClass());

                if (lastModifiedDateProperty != null) {
                    query.setSort(
                            Sort.by(Sort.Order.desc(lastModifiedDateProperty)));
                }
            }

            LoadContext<Object> ctx = new LoadContext<>(metaClass);
            ctx.setQuery(query);
            ctx.setFetchPlan(fetchPan);
            List<Object> objects;
            if(!entitiesLoaders.containsKey(metaClass.getClass())) {
                objects = dataManager.loadList(ctx);
            } else {
                Object bean = entitiesLoaders.get(metaClass.getClass());
                Method method = bean.getClass().getDeclaredMethod(GRAPHQL_ENTITIES_LOADER_METHOD_NAME, LoadContext.class);
                objects = (List<Object>) method.invoke(bean, ctx);
            }
            Set<String> props = environmentUtils.getDotDelimitedProps(environment);
            List<Map<String, Object>> entitiesAsMap = objects.stream()
                    .map(e -> responseBuilder.buildResponse((Entity) e, fetchPan, metaClass, props))
                    .collect(Collectors.toList());

            log.debug("loadEntities return {} objects for {}", entitiesAsMap.size(), metaClass.getName());
            return entitiesAsMap;
        };
    }

    /**
     * Convert graphql orderBy object to jmix format.
     *
     * @param path    - parent property path
     * @param orderBy - graphql orderBy object
     * @return pair that contains propertyPath as key ans SortOrder as value
     */
    @Nullable
    protected Pair<String, Types.SortOrder> buildOrderBy(String path, @Nullable Object orderBy) {
        if (orderBy == null || !Map.class.isAssignableFrom(orderBy.getClass())) {
            return null;
        }

        Map.Entry<String, Object> entry = ((Map<String, Object>) orderBy).entrySet().iterator().next();
        String key = entry.getKey();
        Object valueObj = entry.getValue();
        Class<?> valueClass = valueObj.getClass();

        if (Map.class.isAssignableFrom(valueClass)) {
            return buildOrderBy(key, valueObj);
        }

        String propertyPath = StringUtils.isBlank(path) ? key : path + "." + key;

        if (String.class.isAssignableFrom(valueClass)) {
            String value = (String) valueObj;
            return new ImmutablePair<>(propertyPath, Types.SortOrder.valueOf(value));
        }

        if (Collection.class.isAssignableFrom(valueClass)) {
            valueObj = ((Collection<?>) valueObj).iterator().next();
            if (Types.SortOrder.class.isAssignableFrom(valueObj.getClass())) {
                return new ImmutablePair<>(propertyPath, ((Types.SortOrder) valueObj));
            }
            throw new UnsupportedOperationException("Can't parse orderBy value from value class " + valueObj.getClass());
        }

        throw new UnsupportedOperationException("Can't parse orderBy value from value class " + valueObj.getClass());
    }

    public DataFetcher<?> countEntities(MetaClass metaClass) {
        return environment -> {

            checkCanReadEntity(metaClass);

            Object filter = environment.getArgument(NamingUtils.FILTER);
            log.debug("countEntities: metClass:{}, filter:{}", metaClass, filter);

            LogicalCondition condition = createCondition(filter);
            LoadContext.Query query = generateQuery(metaClass, condition);

            LoadContext<? extends Entity> lc = new LoadContext<>(metaClass);
            lc.setQuery(query);
            long count;
            if(!countLoaders.containsKey(metaClass.getClass())) {
                count = dataManager.getCount(lc);
            } else {
                Object bean = countLoaders.get(metaClass.getClass());
                Method method = bean.getClass().getDeclaredMethod(GRAPHQL_COUNT_LOADER_METHOD_NAME, LoadContext.class);
                count = (Long) method.invoke(bean, lc);
            }
            log.debug("countEntities return {} for {}", count, metaClass.getName());
            return count;
        };
    }

    // todo methods above copypasted from 'jmix-rest'
    protected void checkCanReadEntity(MetaClass metaClass) {
        CrudEntityContext entityContext = applyEntityConstraints(metaClass);
        if (!entityContext.isReadPermitted()) {
            String exceptionMessage = String.format("Reading of the %s is forbidden", metaClass.getName());
            log.warn("checkCanReadEntity: throw exception {}", exceptionMessage);
            throw new GqlEntityValidationException(String.format("Reading of the %s is forbidden", metaClass.getName()));
        }
    }

    protected CrudEntityContext applyEntityConstraints(MetaClass metaClass) {
        CrudEntityContext entityContext = new CrudEntityContext(metaClass);
        accessManager.applyRegisteredConstraints(entityContext);
        return entityContext;
    }

    @Nullable
    protected LogicalCondition createCondition(Object filter) {
        LogicalCondition condition = null;
        if (filter != null && Collection.class.isAssignableFrom(filter.getClass())) {
                /*
                root conditions always aggregated by 'and', 'or' - could be achieved to add the only one 'or' condition in next level
                carList (filter: {OR: [
                      {manufacturer: {EQ: "TESLA"}}
                      {manufacturer: {EQ: "TATA"}}
                    ]})
                 */
            condition = LogicalCondition.and(
                    filterConditionBuilder.buildCollectionOfConditions("", (Collection<Map<String, Object>>) filter)
                            .toArray(new Condition[0]));
        }

        return condition;
    }

    @NotNull
    protected LoadContext.Query generateQuery(MetaClass metaClass, LogicalCondition condition) {
        LoadContext.Query query = new LoadContext.Query("select e from " + metaClass.getName() + " e");
        if (condition != null) {
            query.setCondition(condition);
        }

        return query;
    }

    @PostConstruct
    protected void initCustomLoader() {
        countLoaders = new HashMap<>();
        entityLoaders = new HashMap<>();
        entitiesLoaders = new HashMap<>();
        Map<String, Object> loaders = listableBeanFactory
                .getBeansWithAnnotation(GraphqlLoader.class);
        for(Object loader: loaders.values()) {
           if(loader instanceof GraphqlEntityCountLoader) {
               Method[] methods = loader.getClass().getDeclaredMethods();
               for(Method method: methods) {
                   if(method.getName().equals(GRAPHQL_COUNT_LOADER_METHOD_NAME)) {
                       countLoaders.put(method.getReturnType(), loader);
                   }
               }
           }
           if(loader instanceof GraphqlEntityListLoader) {
                Method[] methods = loader.getClass().getDeclaredMethods();
                for(Method method: methods) {
                    if(method.getName().equals(GRAPHQL_ENTITIES_LOADER_METHOD_NAME)) {
                        entitiesLoaders.put(method.getReturnType(), loader);
                    }
                }
           }
            if(loader instanceof GraphqlEntityLoader) {
                Method[] methods = loader.getClass().getDeclaredMethods();
                for(Method method: methods) {
                    if(method.getName().equals(GRAPHQL_ENTITY_LOADER_METHOD_NAME)) {
                        entityLoaders.put(method.getReturnType(), loader);
                    }
                }
            }
        }

    }
}
