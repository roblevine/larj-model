%% model_builder.pl - Domain Model Construction Utilities
%
%  Provides high-level predicates for building DDD domain models.
%  Wraps the low-level schema predicates with validation and
%  convenience features.
%
%  @author Generated for larj-model
%  @version 1.0

:- module(model_builder, [
    % Context building
    define_context/3,
    add_context_term/3,
    link_contexts/4,

    % Object building by archetype
    define_moment_interval/4,
    add_mi_detail/4,
    add_mi_status_state/3,
    link_plan_actual/3,

    define_role/4,
    link_role_to_player/2,
    link_role_to_mi/2,

    define_party/4,
    define_place/4,
    define_thing/4,
    link_ppt_to_description/2,
    link_ppt_to_role/2,

    define_description/4,
    add_description_default/3,

    % Aggregate building
    define_aggregate/3,
    add_aggregate_member/2,
    add_aggregate_invariant/2,

    % Service building
    define_service/3,
    add_service_operation/4,
    link_service_to_object/2,

    % Domain classification
    mark_core_domain/1,
    mark_supporting_domain/1,
    mark_generic_domain/1,

    % Attribute and behaviour helpers
    add_attribute/4,
    add_behaviour/4,

    % Query helpers
    objects_in_context/2,
    moment_intervals_in_context/2,
    roles_in_context/2,
    ppts_in_context/2,
    descriptions_in_context/2,
    aggregates_in_context/2,
    services_in_context/2
]).

:- use_module(ddd_schema).

%% --------------------------------------------------------------------------
%% Context Building
%% --------------------------------------------------------------------------

%% define_context(+ContextId, +Name, +Scope) is det
%
%  Creates a new bounded context.
%
%  @param ContextId  Unique identifier (atom)
%  @param Name       Human-readable name
%  @param Scope      What this context owns
define_context(ContextId, Name, Scope) :-
    atom(ContextId),
    (   bounded_context(ContextId, _, _)
    ->  throw(error(context_exists(ContextId), context(define_context/3, _)))
    ;   assertz(bounded_context(ContextId, Name, Scope))
    ).

%% add_context_term(+ContextId, +Term, +Meaning) is det
%
%  Adds a term with context-specific meaning.
%
%  @param ContextId  The bounded context
%  @param Term       The domain term
%  @param Meaning    Context-specific meaning
add_context_term(ContextId, Term, Meaning) :-
    bounded_context(ContextId, _, _),
    assertz(context_term(ContextId, Term, Meaning)).
add_context_term(ContextId, _, _) :-
    \+ bounded_context(ContextId, _, _),
    throw(error(unknown_context(ContextId), context(add_context_term/3, _))).

%% link_contexts(+UpstreamId, +DownstreamId, +Pattern, +Notes) is det
%
%  Establishes a relationship between two bounded contexts.
%
%  @param UpstreamId    The upstream context
%  @param DownstreamId  The downstream context
%  @param Pattern       Relationship pattern
%  @param Notes         Additional notes
link_contexts(UpstreamId, DownstreamId, Pattern, Notes) :-
    bounded_context(UpstreamId, _, _),
    bounded_context(DownstreamId, _, _),
    valid_context_pattern(Pattern),
    assertz(context_relationship(UpstreamId, DownstreamId, Pattern, Notes)).

valid_context_pattern(shared_kernel).
valid_context_pattern(customer_supplier).
valid_context_pattern(conformist).
valid_context_pattern(anticorruption_layer).
valid_context_pattern(separate_ways).

%% --------------------------------------------------------------------------
%% Moment-Interval Building
%% --------------------------------------------------------------------------

%% define_moment_interval(+ObjectId, +ContextId, +TemporalType, +TrackedFor) is det
%
%  Creates a new moment-interval domain object.
%
%  @param ObjectId     Unique identifier
%  @param ContextId    The bounded context
%  @param TemporalType One of: moment, interval
%  @param TrackedFor   Business reason for tracking
define_moment_interval(ObjectId, ContextId, TemporalType, TrackedFor) :-
    bounded_context(ContextId, _, _),
    valid_temporal_type(TemporalType),
    \+ moment_interval(ObjectId, _),
    assertz(moment_interval(ObjectId, ContextId)),
    assertz(mi_temporal_type(ObjectId, TemporalType)),
    assertz(mi_attribute(ObjectId, tracked_for, attr(string, true, TrackedFor))).

valid_temporal_type(moment).
valid_temporal_type(interval).

%% add_mi_detail(+ParentMI, +DetailId, +Attributes, +Behaviours) is det
%
%  Adds a detail (line item) to a moment-interval.
%
%  @param ParentMI    The parent moment-interval
%  @param DetailId    Identifier for this detail type
%  @param Attributes  List of attribute specifications
%  @param Behaviours  List of behaviour specifications
add_mi_detail(ParentMI, DetailId, Attributes, Behaviours) :-
    moment_interval(ParentMI, _),
    assertz(mi_detail(ParentMI, DetailId, detail(Attributes, Behaviours))),
    assertz(contains_detail(ParentMI, DetailId)).

%% add_mi_status_state(+ObjectId, +StatusName, +Transitions) is det
%
%  Adds a status state to a moment-interval's lifecycle.
%
%  @param ObjectId    The moment-interval
%  @param StatusName  Name of the status
%  @param Transitions List of valid transitions from this status
add_mi_status_state(ObjectId, StatusName, Transitions) :-
    moment_interval(ObjectId, _),
    assertz(mi_status(ObjectId, StatusName, status(Transitions, ''))).

%% link_plan_actual(+PlanMI, +ActualMI, +RelationType) is det
%
%  Links a planned moment-interval to its actual counterpart.
%
%  @param PlanMI        The planned MI
%  @param ActualMI      The actual MI
%  @param RelationType  Type of relationship
link_plan_actual(PlanMI, ActualMI, RelationType) :-
    moment_interval(PlanMI, _),
    moment_interval(ActualMI, _),
    valid_plan_actual_type(RelationType),
    assertz(mi_plan_actual(PlanMI, ActualMI, RelationType)).

valid_plan_actual_type(plan_to_actual).
valid_plan_actual_type(prior_to_next).
valid_plan_actual_type(generates).

%% --------------------------------------------------------------------------
%% Role Building
%% --------------------------------------------------------------------------

%% define_role(+ObjectId, +ContextId, +RoleContext, +Description) is det
%
%  Creates a new role domain object.
%
%  @param ObjectId     Unique identifier
%  @param ContextId    The bounded context
%  @param RoleContext  What context this role operates in
%  @param Description  Description of the role
define_role(ObjectId, ContextId, RoleContext, Description) :-
    bounded_context(ContextId, _, _),
    \+ role(ObjectId, _),
    assertz(role(ObjectId, ContextId)),
    assertz(role_attribute(ObjectId, role_context, attr(string, true, RoleContext))),
    assertz(role_attribute(ObjectId, description, attr(string, false, Description))).

%% link_role_to_player(+RoleId, +PPTId) is det
%
%  Links a role to the party/place/thing that plays it.
%
%  @param RoleId  The role
%  @param PPTId   The player
link_role_to_player(RoleId, PPTId) :-
    role(RoleId, _),
    party_place_thing(PPTId, _),
    assertz(role_played_by(RoleId, PPTId)),
    assertz(ppt_plays_role(PPTId, RoleId)),
    assertz(plays(PPTId, RoleId)).

%% link_role_to_mi(+RoleId, +MIId) is det
%
%  Links a role to a moment-interval it participates in.
%
%  @param RoleId  The role
%  @param MIId    The moment-interval
link_role_to_mi(RoleId, MIId) :-
    role(RoleId, _),
    moment_interval(MIId, _),
    assertz(role_participates_in(RoleId, MIId)),
    assertz(participates_in(RoleId, MIId, participant)).

%% --------------------------------------------------------------------------
%% Party/Place/Thing Building
%% --------------------------------------------------------------------------

%% define_party(+ObjectId, +ContextId, +PartyType, +Description) is det
%
%  Creates a new party (person or organisation).
%
%  @param ObjectId    Unique identifier
%  @param ContextId   The bounded context
%  @param PartyType   One of: person, organisation
%  @param Description Description of the party
define_party(ObjectId, ContextId, PartyType, Description) :-
    bounded_context(ContextId, _, _),
    valid_party_type(PartyType),
    \+ party_place_thing(ObjectId, _),
    assertz(party_place_thing(ObjectId, ContextId)),
    assertz(ppt_subtype(ObjectId, PartyType)),
    assertz(ppt_attribute(ObjectId, description, attr(string, false, Description))).

valid_party_type(person).
valid_party_type(organisation).
valid_party_type(party).

%% define_place(+ObjectId, +ContextId, +PlaceType, +Description) is det
%
%  Creates a new place (location).
%
%  @param ObjectId    Unique identifier
%  @param ContextId   The bounded context
%  @param PlaceType   Type of place (e.g., warehouse, store)
%  @param Description Description of the place
define_place(ObjectId, ContextId, PlaceType, Description) :-
    bounded_context(ContextId, _, _),
    \+ party_place_thing(ObjectId, _),
    assertz(party_place_thing(ObjectId, ContextId)),
    assertz(ppt_subtype(ObjectId, place)),
    assertz(ppt_attribute(ObjectId, place_type, attr(atom, true, PlaceType))),
    assertz(ppt_attribute(ObjectId, description, attr(string, false, Description))).

%% define_thing(+ObjectId, +ContextId, +ThingType, +Description) is det
%
%  Creates a new thing (physical object or asset).
%
%  @param ObjectId    Unique identifier
%  @param ContextId   The bounded context
%  @param ThingType   Type of thing
%  @param Description Description of the thing
define_thing(ObjectId, ContextId, ThingType, Description) :-
    bounded_context(ContextId, _, _),
    \+ party_place_thing(ObjectId, _),
    assertz(party_place_thing(ObjectId, ContextId)),
    assertz(ppt_subtype(ObjectId, thing)),
    assertz(ppt_attribute(ObjectId, thing_type, attr(atom, true, ThingType))),
    assertz(ppt_attribute(ObjectId, description, attr(string, false, Description))).

%% link_ppt_to_description(+PPTId, +DescriptionId) is det
%
%  Links a party/place/thing to its catalog description.
link_ppt_to_description(PPTId, DescriptionId) :-
    party_place_thing(PPTId, _),
    description(DescriptionId, _),
    assertz(ppt_described_by(PPTId, DescriptionId)),
    assertz(describes(DescriptionId, PPTId)).

%% link_ppt_to_role(+PPTId, +RoleId) is det
%
%  Links a party/place/thing to a role it plays.
link_ppt_to_role(PPTId, RoleId) :-
    party_place_thing(PPTId, _),
    role(RoleId, _),
    assertz(ppt_plays_role(PPTId, RoleId)),
    assertz(role_played_by(RoleId, PPTId)),
    assertz(plays(PPTId, RoleId)).

%% --------------------------------------------------------------------------
%% Description Building
%% --------------------------------------------------------------------------

%% define_description(+ObjectId, +ContextId, +DescribesType, +CatalogInfo) is det
%
%  Creates a new description (catalog entry).
%
%  @param ObjectId      Unique identifier
%  @param ContextId     The bounded context
%  @param DescribesType What type of PPT this describes
%  @param CatalogInfo   Catalog information
define_description(ObjectId, ContextId, DescribesType, CatalogInfo) :-
    bounded_context(ContextId, _, _),
    \+ description(ObjectId, _),
    assertz(description(ObjectId, ContextId)),
    assertz(desc_applies_to(ObjectId, DescribesType)),
    assertz(desc_attribute(ObjectId, catalog_info, attr(string, false, CatalogInfo))).

%% add_description_default(+ObjectId, +AttributeName, +DefaultValue) is det
%
%  Adds a default value to a description.
add_description_default(ObjectId, AttributeName, DefaultValue) :-
    description(ObjectId, _),
    assertz(desc_default_value(ObjectId, AttributeName, DefaultValue)).

%% --------------------------------------------------------------------------
%% Aggregate Building
%% --------------------------------------------------------------------------

%% define_aggregate(+AggregateId, +ContextId, +RootObjectId) is det
%
%  Creates a new aggregate with the specified root.
%
%  @param AggregateId   Unique identifier
%  @param ContextId     The bounded context
%  @param RootObjectId  The aggregate root object
define_aggregate(AggregateId, ContextId, RootObjectId) :-
    bounded_context(ContextId, _, _),
    object_exists(RootObjectId),
    \+ aggregate(AggregateId, _),
    assertz(aggregate(AggregateId, ContextId)),
    assertz(aggregate_root(AggregateId, RootObjectId)).

%% add_aggregate_member(+AggregateId, +MemberObjectId) is det
%
%  Adds a member to an aggregate.
add_aggregate_member(AggregateId, MemberObjectId) :-
    aggregate(AggregateId, _),
    object_exists(MemberObjectId),
    assertz(aggregate_member(AggregateId, MemberObjectId)).

%% add_aggregate_invariant(+AggregateId, +Invariant) is det
%
%  Adds a business invariant to an aggregate.
add_aggregate_invariant(AggregateId, Invariant) :-
    aggregate(AggregateId, _),
    assertz(aggregate_invariant(AggregateId, Invariant)).

%% --------------------------------------------------------------------------
%% Service Building
%% --------------------------------------------------------------------------

%% define_service(+ServiceId, +ContextId, +Description) is det
%
%  Creates a new domain service.
%
%  @param ServiceId    Unique identifier
%  @param ContextId    The bounded context
%  @param Description  Service description
define_service(ServiceId, ContextId, Description) :-
    bounded_context(ContextId, _, _),
    \+ domain_service(ServiceId, _),
    assertz(domain_service(ServiceId, ContextId)),
    assertz(service_operation(ServiceId, description, operation([], [], Description))).

%% add_service_operation(+ServiceId, +OperationName, +Inputs, +Outputs) is det
%
%  Adds an operation to a domain service.
add_service_operation(ServiceId, OperationName, Inputs, Outputs) :-
    domain_service(ServiceId, _),
    assertz(service_operation(ServiceId, OperationName, operation(Inputs, Outputs, ''))).

%% link_service_to_object(+ServiceId, +ObjectId) is det
%
%  Links a service to an object it coordinates.
link_service_to_object(ServiceId, ObjectId) :-
    domain_service(ServiceId, _),
    object_exists(ObjectId),
    assertz(service_coordinates(ServiceId, ObjectId)).

%% --------------------------------------------------------------------------
%% Domain Classification
%% --------------------------------------------------------------------------

%% mark_core_domain(+ObjectOrContextId) is det
%
%  Marks an object or context as core domain.
%  Removes any existing classification first (mutual exclusivity).
mark_core_domain(Id) :-
    (bounded_context(Id, _, _) ; object_exists(Id)),
    retract_domain_classification(Id),
    assertz(core_domain(Id)).

%% mark_supporting_domain(+ObjectOrContextId) is det
%
%  Marks an object or context as supporting domain.
%  Removes any existing classification first (mutual exclusivity).
mark_supporting_domain(Id) :-
    (bounded_context(Id, _, _) ; object_exists(Id)),
    retract_domain_classification(Id),
    assertz(supporting_domain(Id)).

%% mark_generic_domain(+ObjectOrContextId) is det
%
%  Marks an object or context as generic domain.
%  Removes any existing classification first (mutual exclusivity).
mark_generic_domain(Id) :-
    (bounded_context(Id, _, _) ; object_exists(Id)),
    retract_domain_classification(Id),
    assertz(generic_domain(Id)).

%% retract_domain_classification(+Id) is det
%
%  Removes any existing domain classification for the given ID.
%  Always succeeds (ignores if no classification exists).
retract_domain_classification(Id) :-
    (retract(core_domain(Id)) -> true ; true),
    (retract(supporting_domain(Id)) -> true ; true),
    (retract(generic_domain(Id)) -> true ; true).

%% --------------------------------------------------------------------------
%% Attribute and Behaviour Helpers
%% --------------------------------------------------------------------------

%% add_attribute(+ObjectId, +AttributeName, +Type, +Required) is det
%
%  Adds an attribute to any domain object.
add_attribute(ObjectId, AttributeName, Type, Required) :-
    archetype_of(ObjectId, Archetype),
    add_archetype_attribute(Archetype, ObjectId, AttributeName, Type, Required).

add_archetype_attribute(moment_interval, ObjectId, Name, Type, Required) :-
    assertz(mi_attribute(ObjectId, Name, attr(Type, Required, ''))).
add_archetype_attribute(role, ObjectId, Name, Type, Required) :-
    assertz(role_attribute(ObjectId, Name, attr(Type, Required, ''))).
add_archetype_attribute(party_place_thing, ObjectId, Name, Type, Required) :-
    assertz(ppt_attribute(ObjectId, Name, attr(Type, Required, ''))).
add_archetype_attribute(description, ObjectId, Name, Type, Required) :-
    assertz(desc_attribute(ObjectId, Name, attr(Type, Required, ''))).

%% add_behaviour(+ObjectId, +BehaviourName, +Inputs, +Outputs) is det
%
%  Adds a behaviour to any domain object.
add_behaviour(ObjectId, BehaviourName, Inputs, Outputs) :-
    archetype_of(ObjectId, Archetype),
    add_archetype_behaviour(Archetype, ObjectId, BehaviourName, Inputs, Outputs).

add_archetype_behaviour(moment_interval, ObjectId, Name, Inputs, Outputs) :-
    assertz(mi_behaviour(ObjectId, Name, behaviour(Inputs, Outputs, ''))).
add_archetype_behaviour(role, ObjectId, Name, Inputs, Outputs) :-
    assertz(role_behaviour(ObjectId, Name, behaviour(Inputs, Outputs, ''))).
add_archetype_behaviour(party_place_thing, ObjectId, Name, Inputs, Outputs) :-
    assertz(ppt_behaviour(ObjectId, Name, behaviour(Inputs, Outputs, ''))).
add_archetype_behaviour(description, ObjectId, Name, Inputs, Outputs) :-
    assertz(desc_behaviour(ObjectId, Name, behaviour(Inputs, Outputs, ''))).

%% --------------------------------------------------------------------------
%% Query Helpers
%% --------------------------------------------------------------------------

%% object_exists(+ObjectId) is semidet
%
%  Checks if an object exists in any archetype or as an MI-detail.
object_exists(ObjectId) :-
    (   moment_interval(ObjectId, _)
    ;   role(ObjectId, _)
    ;   party_place_thing(ObjectId, _)
    ;   description(ObjectId, _)
    ;   mi_detail(_, ObjectId, _)  % MI-details are also valid objects
    ),
    !.

%% objects_in_context(+ContextId, -Objects) is det
%
%  Returns all domain objects in a bounded context.
objects_in_context(ContextId, Objects) :-
    findall(obj(Id, Archetype), (
        (   moment_interval(Id, ContextId), Archetype = moment_interval
        ;   role(Id, ContextId), Archetype = role
        ;   party_place_thing(Id, ContextId), Archetype = party_place_thing
        ;   description(Id, ContextId), Archetype = description
        )
    ), Objects).

%% moment_intervals_in_context(+ContextId, -MIs) is det
moment_intervals_in_context(ContextId, MIs) :-
    findall(Id, moment_interval(Id, ContextId), MIs).

%% roles_in_context(+ContextId, -Roles) is det
roles_in_context(ContextId, Roles) :-
    findall(Id, role(Id, ContextId), Roles).

%% ppts_in_context(+ContextId, -PPTs) is det
ppts_in_context(ContextId, PPTs) :-
    findall(Id, party_place_thing(Id, ContextId), PPTs).

%% descriptions_in_context(+ContextId, -Descs) is det
descriptions_in_context(ContextId, Descs) :-
    findall(Id, description(Id, ContextId), Descs).

%% aggregates_in_context(+ContextId, -Aggs) is det
aggregates_in_context(ContextId, Aggs) :-
    findall(Id, aggregate(Id, ContextId), Aggs).

%% services_in_context(+ContextId, -Services) is det
services_in_context(ContextId, Services) :-
    findall(Id, domain_service(Id, ContextId), Services).
