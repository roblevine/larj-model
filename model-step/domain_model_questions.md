# Domain Model Review Questions

Typical questions a domain modeller asks when reviewing a domain model, with corresponding Prolog queries to interrogate the model.

---

## 1. Model Inventory

### What bounded contexts exist in the model?

```prolog
?- bounded_context(Id, Name, Scope).
```

### What are all the domain objects grouped by archetype?

```prolog
?- findall(mi(Id), moment_interval(Id, _), MIs),
   findall(role(Id), role(Id, _), Roles),
   findall(ppt(Id), party_place_thing(Id, _), PPTs),
   findall(desc(Id), description(Id, _), Descs).
```

### How many objects of each archetype type are there?

```prolog
?- aggregate_all(count, moment_interval(_, _), MICount),
   aggregate_all(count, role(_, _), RoleCount),
   aggregate_all(count, party_place_thing(_, _), PPTCount),
   aggregate_all(count, description(_, _), DescCount).
```

### What is the archetype classification of a specific object?

```prolog
?- archetype_of(my_object, Archetype).
```

---

## 2. Archetype Classification Validation

### Are there any objects classified under multiple archetypes?

```prolog
?- findall(Id, (
       moment_interval(Id, _),
       (role(Id, _) ; party_place_thing(Id, _) ; description(Id, _))
   ), MultiClassified).
```

### Are there any unclassified concepts mentioned but not formally declared?

```prolog
% Check for referenced but undefined objects
?- findall(Id, (
       (role_played_by(_, Id) ; role_participates_in(_, Id) ;
        ppt_described_by(_, Id) ; desc_applies_to(_, Id)),
       \+ moment_interval(Id, _),
       \+ role(Id, _),
       \+ party_place_thing(Id, _),
       \+ description(Id, _)
   ), Undefined).
```

---

## 3. Moment-Interval Questions

### What are all the moment-intervals and their temporal types?

```prolog
?- findall(mi(Id, Type), (
       moment_interval(Id, _),
       (mi_temporal_type(Id, Type) -> true ; Type = undefined)
   ), MIs).
```

### Which moment-intervals lack a temporal type (moment vs interval)?

```prolog
?- findall(Id, (
       moment_interval(Id, _),
       \+ mi_temporal_type(Id, _)
   ), Missing).
```

### What lifecycle states does each moment-interval have?

```prolog
?- findall(mi(Id, Statuses), (
       moment_interval(Id, _),
       findall(S, mi_status(Id, S, _), Statuses)
   ), MILifecycles).
```

### Which moment-intervals have no lifecycle states defined?

```prolog
?- findall(Id, (
       moment_interval(Id, _),
       \+ mi_status(Id, _, _)
   ), NoLifecycle).
```

### What details (line items) belong to each moment-interval?

```prolog
?- findall(mi(Id, Details), (
       moment_interval(Id, _),
       findall(D, mi_detail(Id, D, _), Details)
   ), MIDetails).
```

### Are there plan/actual pairs defined? Are they complete?

```prolog
?- findall(pair(Plan, Actual, Type),
       mi_plan_actual(Plan, Actual, Type),
   Pairs).

% Check for broken links
?- findall(Plan, (
       mi_plan_actual(Plan, Actual, _),
       \+ moment_interval(Actual, _)
   ), BrokenLinks).
```

### Which moment-intervals have no participants?

```prolog
?- findall(Id, (
       moment_interval(Id, _),
       \+ role_participates_in(_, Id)
   ), Orphans).
```

### What behaviours are defined for each moment-interval?

```prolog
?- findall(mi(Id, Behaviours), (
       moment_interval(Id, _),
       findall(B, mi_behaviour(Id, B, _), Behaviours)
   ), MIBehaviours).
```

---

## 4. Role Questions

### What roles exist and who plays them?

```prolog
?- findall(role(RoleId, Player), (
       role(RoleId, _),
       (role_played_by(RoleId, Player) -> true ; Player = none)
   ), Roles).
```

### Which roles have no player defined?

```prolog
?- findall(Id, (
       role(Id, _),
       \+ role_played_by(Id, _)
   ), OrphanRoles).
```

### What moment-intervals does each role participate in?

```prolog
?- findall(role(RoleId, MIs), (
       role(RoleId, _),
       findall(MI, role_participates_in(RoleId, MI), MIs)
   ), RoleParticipation).
```

### Which roles don't participate in any moment-intervals?

```prolog
?- findall(Id, (
       role(Id, _),
       \+ role_participates_in(Id, _)
   ), InactiveRoles).
```

### For a given moment-interval, what roles participate?

```prolog
?- findall(Role, role_participates_in(Role, my_moment_interval), Participants).
```

---

## 5. Party/Place/Thing Questions

### What are all PPTs and their subtypes?

```prolog
?- findall(ppt(Id, Subtype), (
       party_place_thing(Id, _),
       (ppt_subtype(Id, Subtype) -> true ; Subtype = unspecified)
   ), PPTs).
```

### Which PPTs lack a subtype classification?

```prolog
?- findall(Id, (
       party_place_thing(Id, _),
       \+ ppt_subtype(Id, _)
   ), MissingSubtype).
```

### What roles does each PPT play?

```prolog
?- findall(ppt(Id, Roles), (
       party_place_thing(Id, _),
       findall(R, ppt_plays_role(Id, R), Roles)
   ), PPTRoles).
```

### Which PPTs don't play any roles?

```prolog
?- findall(Id, (
       party_place_thing(Id, _),
       \+ ppt_plays_role(Id, _)
   ), Inactive).
```

### What description applies to each PPT?

```prolog
?- findall(ppt(Id, Desc), (
       party_place_thing(Id, _),
       (ppt_described_by(Id, Desc) -> true ; Desc = none)
   ), PPTDescriptions).
```

### Which PPTs have no description linked?

```prolog
?- findall(Id, (
       party_place_thing(Id, _),
       \+ ppt_described_by(Id, _)
   ), Undescribed).
```

### Do PPTs have identity attributes (identifier, serial number)?

```prolog
?- findall(ppt(Id, HasIdentity), (
       party_place_thing(Id, _),
       (   (ppt_attribute(Id, identifier, _) ;
            ppt_attribute(Id, id, _) ;
            ppt_attribute(Id, serial_number, _))
       ->  HasIdentity = yes
       ;   HasIdentity = no
       )
   ), IdentityCheck).
```

---

## 6. Description Questions

### What descriptions exist and what do they describe?

```prolog
?- findall(desc(Id, AppliesTo), (
       description(Id, _),
       (desc_applies_to(Id, AppliesTo) -> true ; AppliesTo = unlinked)
   ), Descriptions).
```

### Which descriptions have no applies_to link?

```prolog
?- findall(Id, (
       description(Id, _),
       \+ desc_applies_to(Id, _)
   ), Orphans).
```

### What default values does each description provide?

```prolog
?- findall(desc(Id, Defaults), (
       description(Id, _),
       findall(default(Name, Value), desc_default_value(Id, Name, Value), Defaults)
   ), DescDefaults).
```

### Which descriptions have no defaults defined?

```prolog
?- findall(Id, (
       description(Id, _),
       \+ desc_default_value(Id, _, _)
   ), NoDefaults).
```

---

## 7. Link Pattern Validation

### Is the Blue→Green→Yellow→Pink chain complete?

```prolog
% Description → PPT links
?- findall(link(Desc, PPT), ppt_described_by(PPT, Desc), BlueGreen).

% PPT → Role links
?- findall(link(PPT, Role), ppt_plays_role(PPT, Role), GreenYellow).

% Role → MI links
?- findall(link(Role, MI), role_participates_in(Role, MI), YellowPink).
```

### Are there orphan objects not connected to the chain?

```prolog
?- findall(obj(Id, Type), (
       (moment_interval(Id, _), Type = mi ;
        role(Id, _), Type = role ;
        party_place_thing(Id, _), Type = ppt ;
        description(Id, _), Type = desc),
       \+ aggregate_root(_, Id),
       \+ aggregate_member(_, Id)
   ), Orphans).
```

### Which roles are missing both player and MI participation?

```prolog
?- findall(role(Id, Issues), (
       role(Id, _),
       findall(Issue, (
           (\+ role_played_by(Id, _), Issue = no_player) ;
           (\+ role_participates_in(Id, _), Issue = no_mi)
       ), Issues),
       Issues \= []
   ), ProblematicRoles).
```

---

## 8. Aggregate Questions

### What aggregates are defined and what are their roots?

```prolog
?- findall(agg(Id, Root), (
       aggregate(Id, _),
       (aggregate_root(Id, Root) -> true ; Root = none)
   ), Aggregates).
```

### What members belong to each aggregate?

```prolog
?- findall(agg(Id, Root, Members), (
       aggregate(Id, _),
       (aggregate_root(Id, Root) -> true ; Root = none),
       findall(M, aggregate_member(Id, M), Members)
   ), AggStructure).
```

### Which aggregates have no root defined?

```prolog
?- findall(Id, (
       aggregate(Id, _),
       \+ aggregate_root(Id, _)
   ), NoRoot).
```

### Are MI details inside the same aggregate as their parent MI?

```prolog
?- findall(issue(MI, Detail), (
       mi_detail(MI, Detail, _),
       aggregate_root(MIAgg, MI),
       \+ aggregate_member(MIAgg, Detail)
   ), CohesionIssues).
```

### What invariants are defined for each aggregate?

```prolog
?- findall(agg(Id, Invariants), (
       aggregate(Id, _),
       findall(I, aggregate_invariant(Id, I), Invariants)
   ), Invariants).
```

### Which aggregates have no invariants?

```prolog
?- findall(Id, (
       aggregate(Id, _),
       \+ aggregate_invariant(Id, _)
   ), NoInvariants).
```

---

## 9. Bounded Context Questions

### What terms have context-specific meanings?

```prolog
?- findall(term(Context, Term, Meaning),
       context_term(Context, Term, Meaning),
   Terms).
```

### Does the same term appear with different meanings in different contexts?

```prolog
?- findall(term(Term, Contexts), (
       context_term(_, Term, _),
       findall(ctx(C, M), context_term(C, Term, M), Contexts),
       length(Contexts, L),
       L > 1
   ), AmbiguousTerms).
```

### What are the relationships between bounded contexts?

```prolog
?- findall(rel(Upstream, Downstream, Pattern, Notes),
       context_relationship(Upstream, Downstream, Pattern, Notes),
   Relationships).
```

### Which contexts use the anti-corruption layer pattern?

```prolog
?- findall(rel(Up, Down),
       context_relationship(Up, Down, anticorruption_layer, _),
   ACLRelationships).
```

---

## 10. Core Domain Questions

### What is classified as core domain?

```prolog
?- findall(Id, core_domain(Id), CoreDomain).
```

### What is classified as supporting domain?

```prolog
?- findall(Id, supporting_domain(Id), Supporting).
```

### What is classified as generic domain?

```prolog
?- findall(Id, generic_domain(Id), Generic).
```

### Are moment-intervals (business transactions) in the core domain?

```prolog
?- findall(mi(Id, InCore), (
       moment_interval(Id, _),
       (core_domain(Id) -> InCore = yes ; InCore = no)
   ), MIClassification).
```

---

## 11. Common Error Detection

### Are there things with temporal attributes that should be moment-intervals?

```prolog
?- findall(Id, (
       party_place_thing(Id, _),
       ppt_subtype(Id, thing),
       (ppt_attribute(Id, date, _) ;
        ppt_attribute(Id, datetime, _) ;
        ppt_attribute(Id, status, _))
   ), SuspectThings).
```

### Are there PPTs directly participating in MIs without roles?

```prolog
?- findall(issue(PPT, MI), (
       participates_in(PPT, MI, _),
       party_place_thing(PPT, _),
       moment_interval(MI, _)
   ), MissingRoleSeparation).
```

### Are there aggregates containing multiple moment-intervals?

```prolog
?- findall(agg(Id, MIs), (
       aggregate(Id, _),
       findall(MI, (
           (aggregate_root(Id, MI) ; aggregate_member(Id, MI)),
           moment_interval(MI, _)
       ), MIs),
       length(MIs, L),
       L > 1
   ), OversizedAggregates).
```

### Are there descriptions with unique identifiers (should be things)?

```prolog
?- findall(Id, (
       description(Id, _),
       (desc_attribute(Id, serial_number, _) ;
        desc_attribute(Id, unique_id, _))
   ), SuspectDescriptions).
```

---

## 12. Domain Services

### What domain services are defined?

```prolog
?- findall(svc(Id, Context), domain_service(Id, Context), Services).
```

### What operations does each service provide?

```prolog
?- findall(svc(Id, Ops), (
       domain_service(Id, _),
       findall(Op, service_operation(Id, Op, _), Ops)
   ), ServiceOps).
```

### What objects does each service coordinate?

```prolog
?- findall(svc(Id, Coordinates), (
       domain_service(Id, _),
       findall(Obj, service_coordinates(Id, Obj), Coordinates)
   ), ServiceCoordination).
```

---

## 13. Inheritance and Refactoring Analysis

### What inheritance relationships exist in the model?

```prolog
?- findall(inherits(Child, Parent),
       inherits_from(Child, Parent),
   Relationships).
```

### What is the full inheritance chain for an object?

```prolog
?- inheritance_chain(my_object, Chain).
```

### Which objects are marked as abstract (base types)?

```prolog
?- findall(Id, abstract_object(Id), AbstractObjects).
```

### What are all descendants of a given base object?

```prolog
?- descendants(base_object, Descendants).
```

### Which objects have no parent (root of hierarchy or standalone)?

```prolog
?- findall(Id, (
       all_objects(Id),
       \+ inherits_from(Id, _)
   ), RootObjects).
```

### Which objects have multiple levels of inheritance?

```prolog
?- findall(obj(Id, Depth), (
       all_objects(Id),
       inheritance_chain(Id, Chain),
       length(Chain, Depth),
       Depth > 1
   ), DeepHierarchies).
```

---

## 14. Refactoring Opportunity Detection

### What attributes are shared between two objects?

```prolog
?- common_attributes(object_a, object_b, SharedAttrs).
```

### What behaviours are shared between two objects?

```prolog
?- common_behaviours(object_a, object_b, SharedBehavs).
```

### Which object pairs are candidates for extracting a common base type?

```prolog
?- refactor_candidates(Candidates).
% Returns: candidate(Obj1, Obj2, SharedAttrs, SharedBehavs)
% where SharedAttrs >= 2 or SharedBehavs >= 1
```

### Find all objects sharing a specific attribute name (potential generalisation)?

```prolog
?- findall(Id, object_attribute(Id, status, _), ObjectsWithStatus).
```

### Find all objects sharing a specific behaviour name?

```prolog
?- findall(Id, object_behaviour(Id, validate, _), ObjectsWithValidate).
```

### What attributes appear across multiple objects (extraction candidates)?

```prolog
?- findall(attr(Name, Count, Objects), (
       object_attribute(_, Name, _),
       findall(Id, object_attribute(Id, Name, _), Ids),
       sort(Ids, Objects),
       length(Objects, Count),
       Count > 1
   ), SharedAttributes),
   sort(SharedAttributes, Unique).
```

### What behaviours appear across multiple objects (extraction candidates)?

```prolog
?- findall(behav(Name, Count, Objects), (
       object_behaviour(_, Name, _),
       findall(Id, object_behaviour(Id, Name, _), Ids),
       sort(Ids, Objects),
       length(Objects, Count),
       Count > 1
   ), SharedBehaviours),
   sort(SharedBehaviours, Unique).
```

### Could a refactoring reduce duplication? (feasibility check)

```prolog
% Find pairs with significant overlap that don't already share a parent
?- findall(opportunity(Obj1, Obj2, Attrs, Behavs, Savings), (
       refactor_candidates(Candidates),
       member(candidate(Obj1, Obj2, Attrs, Behavs), Candidates),
       length(Attrs, AttrCount),
       length(Behavs, BehavCount),
       Savings is AttrCount + BehavCount,
       Savings >= 3
   ), Opportunities).
```

### Are there circular inheritance relationships (error)?

```prolog
?- findall(cycle(A, B), (
       inherits_from(A, B),
       inheritance_chain(B, Chain),
       member(A, Chain)
   ), Cycles).
```

### Which abstract objects have no concrete descendants?

```prolog
?- findall(Id, (
       abstract_object(Id),
       descendants(Id, Desc),
       Desc = []
   ), UnusedAbstracts).
```

---

## 15. Comprehensive Validation

### Run full model validation

```prolog
?- validate_full_model(Results),
   findall(Check, member(result(Check, fail, _), Results), Failures).
```

### Generate a validation report

```prolog
?- generate_validation_report('MyModel', Report),
   write(Report).
```

### List all validation issues in one query

```prolog
?- detect_mi_as_thing(A),
   detect_role_without_player(B),
   detect_oversized_aggregate(C),
   detect_missing_role_separation(D),
   list_orphan_objects(E),
   Issues = [mi_as_thing(A), role_no_player(B), oversized_agg(C),
             missing_role(D), orphans(E)].
```

---

## Quick Reference: Key Predicates

| Question Type | Primary Predicate |
|---------------|-------------------|
| Object classification | `archetype_of/2` |
| MI temporal nature | `mi_temporal_type/2` |
| MI lifecycle | `mi_status/3` |
| MI details | `mi_detail/3` |
| Role players | `role_played_by/2` |
| Role participation | `role_participates_in/2` |
| PPT subtype | `ppt_subtype/2` |
| PPT roles | `ppt_plays_role/2` |
| PPT description | `ppt_described_by/2` |
| Description target | `desc_applies_to/2` |
| Aggregate structure | `aggregate_root/2`, `aggregate_member/2` |
| Context terms | `context_term/3` |
| Context relationships | `context_relationship/4` |
| Inheritance | `inherits_from/2`, `inheritance_chain/2` |
| Abstract types | `abstract_object/1` |
| Shared attributes | `common_attributes/3`, `object_attribute/3` |
| Shared behaviours | `common_behaviours/3`, `object_behaviour/3` |
| Refactoring candidates | `refactor_candidates/1` |
| Descendants | `descendants/2` |
| Full validation | `validate_full_model/1` |
