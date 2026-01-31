%% ddd_schema.pl - Domain-Driven Design Model Schema
%
%  Prolog representation of DDD bounded contexts and domain objects
%  using archetype-based classification (Moment-Interval, Role,
%  Party/Place/Thing, Description).
%
%  @author Generated for larj-model
%  @version 1.0

:- module(ddd_schema, [
    % Bounded Context predicates
    bounded_context/3,
    context_term/3,
    context_relationship/4,

    % Archetype classification predicates
    moment_interval/2,
    role/2,
    party_place_thing/2,
    description/2,

    % Domain object detail predicates
    mi_attribute/3,
    mi_behaviour/3,
    mi_detail/3,
    mi_status/3,
    mi_temporal_type/2,
    mi_plan_actual/3,

    role_attribute/3,
    role_behaviour/3,
    role_played_by/2,
    role_participates_in/2,

    ppt_attribute/3,
    ppt_behaviour/3,
    ppt_subtype/2,
    ppt_described_by/2,
    ppt_plays_role/2,

    desc_attribute/3,
    desc_behaviour/3,
    desc_default_value/3,
    desc_applies_to/2,

    % Aggregate predicates
    aggregate/2,
    aggregate_root/2,
    aggregate_member/2,
    aggregate_invariant/2,

    % Domain service predicates
    domain_service/2,
    service_operation/3,
    service_coordinates/2,

    % Relationship predicates
    participates_in/3,
    contains_detail/2,
    describes/2,
    plays/2,

    % Core domain classification
    core_domain/1,
    supporting_domain/1,
    generic_domain/1,

    % Validation predicates
    validate_model/1,
    archetype_of/2,

    % Model management
    clear_model/0
]).

%% --------------------------------------------------------------------------
%% Bounded Context Schema
%% --------------------------------------------------------------------------

%% bounded_context(+ContextId, +Name, +Scope) is nondet
%
%  Defines a bounded context within the domain model.
%
%  @param ContextId  Unique identifier for the context (atom)
%  @param Name       Human-readable name (atom or string)
%  @param Scope      Description of what this context owns (atom or string)
:- dynamic bounded_context/3.

%% context_term(+ContextId, +Term, +Meaning) is nondet
%
%  Defines a term with context-specific meaning within a bounded context.
%  The same term may have different meanings in different contexts.
%
%  @param ContextId  The bounded context this term belongs to
%  @param Term       The domain term (atom)
%  @param Meaning    Context-specific meaning (atom or string)
:- dynamic context_term/3.

%% context_relationship(+UpstreamContext, +DownstreamContext, +Pattern, +Notes) is nondet
%
%  Defines the relationship between two bounded contexts.
%
%  @param UpstreamContext    The upstream context identifier
%  @param DownstreamContext  The downstream context identifier
%  @param Pattern            One of: shared_kernel, customer_supplier,
%                            conformist, anticorruption_layer, separate_ways
%  @param Notes              Additional notes about the relationship
:- dynamic context_relationship/4.

%% --------------------------------------------------------------------------
%% Archetype Classification Schema
%% --------------------------------------------------------------------------

%% moment_interval(+ObjectId, +ContextId) is nondet
%
%  Declares an object as a Moment-Interval archetype (Pink).
%  Moment-intervals represent points or periods in time that the
%  system tracks for business or legal reasons.
%
%  @param ObjectId   Unique identifier for the domain object
%  @param ContextId  The bounded context this object belongs to
:- dynamic moment_interval/2.

%% role(+ObjectId, +ContextId) is nondet
%
%  Declares an object as a Role archetype (Yellow).
%  Roles represent ways that parties, places, or things participate
%  in the domain.
%
%  @param ObjectId   Unique identifier for the domain object
%  @param ContextId  The bounded context this object belongs to
:- dynamic role/2.

%% party_place_thing(+ObjectId, +ContextId) is nondet
%
%  Declares an object as a Party/Place/Thing archetype (Green).
%  These are entities with independent existence that play roles.
%
%  @param ObjectId   Unique identifier for the domain object
%  @param ContextId  The bounded context this object belongs to
:- dynamic party_place_thing/2.

%% description(+ObjectId, +ContextId) is nondet
%
%  Declares an object as a Description archetype (Blue).
%  Descriptions are catalog-entry-like objects that provide
%  values applying to multiple instances.
%
%  @param ObjectId   Unique identifier for the domain object
%  @param ContextId  The bounded context this object belongs to
:- dynamic description/2.

%% --------------------------------------------------------------------------
%% Moment-Interval Detail Schema
%% --------------------------------------------------------------------------

%% mi_temporal_type(+ObjectId, +TemporalType) is nondet
%
%  Specifies whether a moment-interval is a point in time or a duration.
%
%  @param ObjectId      The moment-interval object
%  @param TemporalType  One of: moment, interval
:- dynamic mi_temporal_type/2.

%% mi_attribute(+ObjectId, +AttributeName, +AttributeSpec) is nondet
%
%  Defines an attribute of a moment-interval.
%  AttributeSpec is a compound term describing the attribute.
%
%  @param ObjectId       The moment-interval object
%  @param AttributeName  Name of the attribute (atom)
%  @param AttributeSpec  Specification: attr(Type, Required, Description)
:- dynamic mi_attribute/3.

%% mi_behaviour(+ObjectId, +BehaviourName, +BehaviourSpec) is nondet
%
%  Defines a behaviour (operation) of a moment-interval.
%
%  @param ObjectId       The moment-interval object
%  @param BehaviourName  Name of the behaviour (atom)
%  @param BehaviourSpec  Specification: behaviour(Inputs, Outputs, Description)
:- dynamic mi_behaviour/3.

%% mi_detail(+ParentMI, +DetailId, +DetailSpec) is nondet
%
%  Defines a detail (line item) that belongs to a moment-interval.
%  Details are components that contribute to the parent's totals.
%
%  @param ParentMI    The parent moment-interval
%  @param DetailId    Identifier for the detail type
%  @param DetailSpec  Specification: detail(Attributes, Behaviours)
:- dynamic mi_detail/3.

%% mi_status(+ObjectId, +StatusName, +StatusSpec) is nondet
%
%  Defines a status state for a moment-interval's lifecycle.
%
%  @param ObjectId    The moment-interval object
%  @param StatusName  Name of the status (atom)
%  @param StatusSpec  Specification: status(Transitions, Description)
:- dynamic mi_status/3.

%% mi_plan_actual(+PlanMI, +ActualMI, +RelationType) is nondet
%
%  Links a planned moment-interval to its actual counterpart.
%
%  @param PlanMI        The planned moment-interval
%  @param ActualMI      The actual moment-interval
%  @param RelationType  One of: plan_to_actual, prior_to_next, generates
:- dynamic mi_plan_actual/3.

%% --------------------------------------------------------------------------
%% Role Detail Schema
%% --------------------------------------------------------------------------

%% role_attribute(+ObjectId, +AttributeName, +AttributeSpec) is nondet
%
%  Defines an attribute of a role.
%
%  @param ObjectId       The role object
%  @param AttributeName  Name of the attribute
%  @param AttributeSpec  Specification: attr(Type, Required, Description)
:- dynamic role_attribute/3.

%% role_behaviour(+ObjectId, +BehaviourName, +BehaviourSpec) is nondet
%
%  Defines a behaviour of a role.
%
%  @param ObjectId       The role object
%  @param BehaviourName  Name of the behaviour
%  @param BehaviourSpec  Specification: behaviour(Inputs, Outputs, Description)
:- dynamic role_behaviour/3.

%% role_played_by(+RoleId, +PPTId) is nondet
%
%  Declares which party/place/thing plays this role.
%
%  @param RoleId  The role object
%  @param PPTId   The party/place/thing that plays this role
:- dynamic role_played_by/2.

%% role_participates_in(+RoleId, +MIId) is nondet
%
%  Declares which moment-intervals this role participates in.
%
%  @param RoleId  The role object
%  @param MIId    The moment-interval participated in
:- dynamic role_participates_in/2.

%% --------------------------------------------------------------------------
%% Party/Place/Thing Detail Schema
%% --------------------------------------------------------------------------

%% ppt_subtype(+ObjectId, +Subtype) is nondet
%
%  Specifies the subtype of a party/place/thing.
%
%  @param ObjectId  The PPT object
%  @param Subtype   One of: person, organisation, party, place, thing
:- dynamic ppt_subtype/2.

%% ppt_attribute(+ObjectId, +AttributeName, +AttributeSpec) is nondet
%
%  Defines an attribute of a party/place/thing.
%
%  @param ObjectId       The PPT object
%  @param AttributeName  Name of the attribute
%  @param AttributeSpec  Specification: attr(Type, Required, Description)
:- dynamic ppt_attribute/3.

%% ppt_behaviour(+ObjectId, +BehaviourName, +BehaviourSpec) is nondet
%
%  Defines a behaviour of a party/place/thing.
%
%  @param ObjectId       The PPT object
%  @param BehaviourName  Name of the behaviour
%  @param BehaviourSpec  Specification: behaviour(Inputs, Outputs, Description)
:- dynamic ppt_behaviour/3.

%% ppt_described_by(+PPTId, +DescriptionId) is nondet
%
%  Links a party/place/thing to its catalog description.
%
%  @param PPTId          The PPT object
%  @param DescriptionId  The description that applies to this PPT
:- dynamic ppt_described_by/2.

%% ppt_plays_role(+PPTId, +RoleId) is nondet
%
%  Declares which roles this party/place/thing can play.
%
%  @param PPTId   The PPT object
%  @param RoleId  A role this PPT can play
:- dynamic ppt_plays_role/2.

%% --------------------------------------------------------------------------
%% Description Detail Schema
%% --------------------------------------------------------------------------

%% desc_attribute(+ObjectId, +AttributeName, +AttributeSpec) is nondet
%
%  Defines an attribute of a description.
%
%  @param ObjectId       The description object
%  @param AttributeName  Name of the attribute
%  @param AttributeSpec  Specification: attr(Type, Required, Description)
:- dynamic desc_attribute/3.

%% desc_behaviour(+ObjectId, +BehaviourName, +BehaviourSpec) is nondet
%
%  Defines a behaviour of a description.
%
%  @param ObjectId       The description object
%  @param BehaviourName  Name of the behaviour
%  @param BehaviourSpec  Specification: behaviour(Inputs, Outputs, Description)
:- dynamic desc_behaviour/3.

%% desc_default_value(+ObjectId, +AttributeName, +DefaultValue) is nondet
%
%  Defines a default value provided by a description.
%
%  @param ObjectId       The description object
%  @param AttributeName  The attribute this default applies to
%  @param DefaultValue   The default value
:- dynamic desc_default_value/3.

%% desc_applies_to(+DescriptionId, +PPTType) is nondet
%
%  Declares what type of party/place/thing this description applies to.
%
%  @param DescriptionId  The description object
%  @param PPTType        The type of PPT this describes
:- dynamic desc_applies_to/2.

%% --------------------------------------------------------------------------
%% Aggregate Schema
%% --------------------------------------------------------------------------

%% aggregate(+AggregateId, +ContextId) is nondet
%
%  Declares an aggregate within a bounded context.
%
%  @param AggregateId  Unique identifier for the aggregate
%  @param ContextId    The bounded context containing this aggregate
:- dynamic aggregate/2.

%% aggregate_root(+AggregateId, +RootObjectId) is nondet
%
%  Declares the root entity of an aggregate.
%
%  @param AggregateId   The aggregate
%  @param RootObjectId  The domain object serving as root
:- dynamic aggregate_root/2.

%% aggregate_member(+AggregateId, +MemberObjectId) is nondet
%
%  Declares a non-root member of an aggregate.
%
%  @param AggregateId     The aggregate
%  @param MemberObjectId  A domain object within the aggregate boundary
:- dynamic aggregate_member/2.

%% aggregate_invariant(+AggregateId, +Invariant) is nondet
%
%  Declares a business invariant maintained by the aggregate.
%
%  @param AggregateId  The aggregate
%  @param Invariant    Description of the invariant rule
:- dynamic aggregate_invariant/2.

%% --------------------------------------------------------------------------
%% Domain Service Schema
%% --------------------------------------------------------------------------

%% domain_service(+ServiceId, +ContextId) is nondet
%
%  Declares a domain service within a bounded context.
%
%  @param ServiceId  Unique identifier for the service
%  @param ContextId  The bounded context containing this service
:- dynamic domain_service/2.

%% service_operation(+ServiceId, +OperationName, +OperationSpec) is nondet
%
%  Defines an operation provided by a domain service.
%
%  @param ServiceId      The service
%  @param OperationName  Name of the operation
%  @param OperationSpec  Specification: operation(Inputs, Outputs, Rules)
:- dynamic service_operation/3.

%% service_coordinates(+ServiceId, +ObjectId) is nondet
%
%  Declares which domain objects a service coordinates.
%
%  @param ServiceId  The service
%  @param ObjectId   A domain object coordinated by this service
:- dynamic service_coordinates/2.

%% --------------------------------------------------------------------------
%% Core Domain Classification Schema
%% --------------------------------------------------------------------------

%% core_domain(+ObjectOrContextId) is nondet
%
%  Marks an object or context as part of the core domain.
%  Core domain provides competitive advantage; requires custom development.
:- dynamic core_domain/1.

%% supporting_domain(+ObjectOrContextId) is nondet
%
%  Marks an object or context as supporting domain.
%  Necessary but not differentiating.
:- dynamic supporting_domain/1.

%% generic_domain(+ObjectOrContextId) is nondet
%
%  Marks an object or context as generic domain.
%  Standard problem; use off-the-shelf solutions.
:- dynamic generic_domain/1.

%% --------------------------------------------------------------------------
%% Convenience Relationship Predicates
%% --------------------------------------------------------------------------

%% participates_in(+RoleId, +MIId, +ParticipationType) is nondet
%
%  Extended participation relationship with type annotation.
%
%  @param RoleId            The participating role
%  @param MIId              The moment-interval
%  @param ParticipationType Role in the interaction (e.g., initiator, recipient)
:- dynamic participates_in/3.

%% contains_detail(+MIId, +DetailId) is nondet
%
%  Declares that a moment-interval contains a detail.
:- dynamic contains_detail/2.

%% describes(+DescriptionId, +PPTId) is nondet
%
%  Declares that a description describes a PPT instance.
:- dynamic describes/2.

%% plays(+PPTId, +RoleId) is nondet
%
%  Declares that a PPT plays a role.
:- dynamic plays/2.

%% --------------------------------------------------------------------------
%% Archetype Query Predicates
%% --------------------------------------------------------------------------

%% archetype_of(+ObjectId, -Archetype) is semidet
%
%  Determines the archetype classification of a domain object.
%
%  @param ObjectId   The domain object to classify
%  @param Archetype  One of: moment_interval, role, party_place_thing, description
archetype_of(ObjectId, moment_interval) :-
    moment_interval(ObjectId, _),
    !.
archetype_of(ObjectId, role) :-
    role(ObjectId, _),
    !.
archetype_of(ObjectId, party_place_thing) :-
    party_place_thing(ObjectId, _),
    !.
archetype_of(ObjectId, description) :-
    description(ObjectId, _),
    !.

%% --------------------------------------------------------------------------
%% Validation Predicates
%% --------------------------------------------------------------------------

%% validate_model(+ContextId) is semidet
%
%  Validates the domain model for a bounded context.
%  Checks for common errors and structural integrity.
%
%  @param ContextId  The bounded context to validate
validate_model(ContextId) :-
    bounded_context(ContextId, _, _),
    validate_archetype_links(ContextId),
    validate_aggregate_boundaries(ContextId),
    validate_role_players(ContextId),
    validate_mi_lifecycle(ContextId).

% validate_archetype_links(+ContextId) is semidet
%
%  Validates that archetype links follow the standard pattern:
%  Description → PPT → Role → Moment-Interval
validate_archetype_links(ContextId) :-
    % All roles must have players
    forall(
        role(RoleId, ContextId),
        role_played_by(RoleId, _)
    ),
    % All PPTs described by descriptions must have valid descriptions
    forall(
        (party_place_thing(PPTId, ContextId), ppt_described_by(PPTId, DescId)),
        description(DescId, ContextId)
    ).

% validate_aggregate_boundaries(+ContextId) is semidet
%
%  Validates aggregate structure.
validate_aggregate_boundaries(ContextId) :-
    % Every aggregate must have exactly one root
    forall(
        aggregate(AggId, ContextId),
        aggregate_root(AggId, _)
    ).

% validate_role_players(+ContextId) is semidet
%
%  Validates that roles are played by valid PPTs.
validate_role_players(ContextId) :-
    forall(
        (role(RoleId, ContextId), role_played_by(RoleId, PPTId)),
        party_place_thing(PPTId, _)
    ).

% validate_mi_lifecycle(+ContextId) is semidet
%
%  Validates that moment-intervals have temporal types.
validate_mi_lifecycle(ContextId) :-
    forall(
        moment_interval(MIId, ContextId),
        mi_temporal_type(MIId, _)
    ).

%% --------------------------------------------------------------------------
%% Model Reset
%% --------------------------------------------------------------------------

%% clear_model is det
%
%  Clears all dynamic facts from the model.
clear_model :-
    retractall(bounded_context(_, _, _)),
    retractall(context_term(_, _, _)),
    retractall(context_relationship(_, _, _, _)),
    retractall(moment_interval(_, _)),
    retractall(role(_, _)),
    retractall(party_place_thing(_, _)),
    retractall(description(_, _)),
    retractall(mi_temporal_type(_, _)),
    retractall(mi_attribute(_, _, _)),
    retractall(mi_behaviour(_, _, _)),
    retractall(mi_detail(_, _, _)),
    retractall(mi_status(_, _, _)),
    retractall(mi_plan_actual(_, _, _)),
    retractall(role_attribute(_, _, _)),
    retractall(role_behaviour(_, _, _)),
    retractall(role_played_by(_, _)),
    retractall(role_participates_in(_, _)),
    retractall(ppt_subtype(_, _)),
    retractall(ppt_attribute(_, _, _)),
    retractall(ppt_behaviour(_, _, _)),
    retractall(ppt_described_by(_, _)),
    retractall(ppt_plays_role(_, _)),
    retractall(desc_attribute(_, _, _)),
    retractall(desc_behaviour(_, _, _)),
    retractall(desc_default_value(_, _,_)),
    retractall(desc_applies_to(_, _)),
    retractall(aggregate(_, _)),
    retractall(aggregate_root(_, _)),
    retractall(aggregate_member(_, _)),
    retractall(aggregate_invariant(_, _)),
    retractall(domain_service(_, _)),
    retractall(service_operation(_, _, _)),
    retractall(service_coordinates(_, _)),
    retractall(core_domain(_)),
    retractall(supporting_domain(_)),
    retractall(generic_domain(_)),
    retractall(participates_in(_, _, _)),
    retractall(contains_detail(_, _)),
    retractall(describes(_, _)),
    retractall(plays(_, _)).
