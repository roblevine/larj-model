# SPEC.md - larj-model Specification

## Overview

**larj-model** is a Prolog-based domain modeling toolkit that:
1. Accepts domain context and process descriptions as input
2. Models software using DDD (Domain-Driven Design) archetype principles
3. Visualizes object models using Graphviz
4. Exports language-agnostic model representations for implementation in any target language

The Prolog model serves as a canonical representation that can be exported to JSON, YAML, or language-specific formats for implementation.

---

## REQ-001: Domain Input Processing

The system shall accept domain knowledge input and parse it into a structured internal representation.

### Description

Domain analysts and developers provide domain context descriptions using a Prolog-based DSL. The system loads, validates, and stores these definitions as dynamic facts that form the domain model.

### Acceptance Criteria

#### AC-001-01: Load Prolog DSL Files
- Given a valid Prolog file containing model-building predicates
- When `process_domain_model/2` is called with the file path
- Then all predicates are executed and facts are asserted into the schema

#### AC-001-02: Clear Existing Model
- Given a model already loaded in memory
- When `clear_model/0` is called
- Then all dynamic facts from previous models are retracted

#### AC-001-03: File Not Found Error
- Given a non-existent file path
- When `load_domain_input/1` is called
- Then an informative error is thrown: `error(file_not_found(Path), _)`

#### AC-001-04: Duplicate Context Detection
- Given a context with ID `ctx_a` already exists
- When `define_context(ctx_a, _, _)` is called again
- Then an error is thrown indicating duplicate context

### Test Mapping
- `test/test_schema.pl`: `define_context_creates_context`
- `test/test_schema.pl`: `define_context_fails_on_duplicate`

---

## REQ-002: Bounded Context Management

The system shall support defining and relating bounded contexts as per DDD strategic design.

### Description

Bounded contexts define linguistic boundaries where terms have specific meanings. The system supports five relationship patterns between contexts: Shared Kernel, Customer/Supplier, Conformist, Anti-Corruption Layer, and Separate Ways.

### Acceptance Criteria

#### AC-002-01: Define Bounded Context
- Given valid context parameters (ID, Name, Scope)
- When `define_context/3` is called
- Then a `bounded_context/3` fact is asserted

#### AC-002-02: Add Context Terminology
- Given an existing bounded context
- When `add_context_term/3` is called with term and meaning
- Then a `context_term/3` fact is asserted linking term to context

#### AC-002-03: Link Contexts with Relationship Pattern
- Given two existing bounded contexts
- When `link_contexts/4` is called with upstream, downstream, pattern, notes
- Then a `context_relationship/4` fact is asserted
- And pattern must be one of: `shared_kernel`, `customer_supplier`, `conformist`, `anticorruption_layer`, `separate_ways`

#### AC-002-04: Query Contexts
- Given multiple bounded contexts defined
- When querying `bounded_context(Id, Name, Scope)`
- Then all matching contexts are enumerable via backtracking

### Test Mapping
- `test/test_schema.pl`: `add_context_term_adds_term`
- `test/test_schema.pl`: `link_contexts_creates_relationship`

---

## REQ-003: Four-Archetype Classification System

The system shall classify all domain objects into exactly one of four archetypes: Moment-Interval (Pink), Role (Yellow), Party/Place/Thing (Green), or Description (Blue).

### Description

The archetype system ensures systematic coverage of domain concepts:
- **Moment-Interval**: Temporal business events that the system must track
- **Role**: How parties participate in business events
- **Party/Place/Thing**: Entities with unique identity
- **Description**: Catalog/template entries that describe other objects

### Acceptance Criteria

#### AC-003-01: Define Moment-Interval
- Given a valid context exists
- When `define_moment_interval/4` is called with ID, context, temporal_type, tracked_for
- Then `moment_interval/2` and `mi_temporal_type/2` facts are asserted
- And temporal_type must be `moment` or `interval`

#### AC-003-02: Define Role
- Given a valid context exists
- When `define_role/4` is called with ID, context, role_context, description
- Then `role/2` fact is asserted

#### AC-003-03: Define Party/Place/Thing
- Given a valid context exists
- When `define_party/4`, `define_place/4`, or `define_thing/4` is called
- Then `party_place_thing/2` and `ppt_subtype/2` facts are asserted
- And subtype reflects the specific type (person, organisation, place, thing)

#### AC-003-04: Define Description
- Given a valid context exists
- When `define_description/4` is called with ID, context, describes_type, catalog_info
- Then `description/2` and `desc_applies_to/2` facts are asserted

#### AC-003-05: Archetype Inference
- Given any domain object ID
- When `archetype_of/2` is called
- Then exactly one archetype is returned: `moment_interval`, `role`, `party_place_thing`, or `description`

### Test Mapping
- `test/test_schema.pl`: `define_moment_interval_creates_mi`
- `test/test_schema.pl`: `define_role_creates_role`
- `test/test_schema.pl`: `define_party_creates_ppt`, `define_place_creates_ppt`, `define_thing_creates_ppt`
- `test/test_schema.pl`: `define_description_creates_desc`
- `test/test_schema.pl`: `archetype_of_moment_interval`, `archetype_of_role`, `archetype_of_ppt`, `archetype_of_description`

---

## REQ-004: Archetype Linking Pattern

The system shall enforce the standard DDD archetype linking pattern: Description → PPT → Role → Moment-Interval.

### Description

Objects are linked following this directional pattern:
```
Blue (Description) ──describes──▶ Green (PPT) ──plays──▶ Yellow (Role) ──participates──▶ Pink (MI)
                                                                                              │
                                                                                              ▼
                                                                                          MI-Detail
```

### Acceptance Criteria

#### AC-004-01: Link Description to PPT
- Given a description and a party/place/thing exist
- When `link_ppt_to_description/2` is called
- Then `ppt_described_by/2` fact is asserted

#### AC-004-02: Link PPT to Role
- Given a party/place/thing and a role exist
- When `link_ppt_to_role/2` or `link_role_to_player/2` is called
- Then both `ppt_plays_role/2` and `role_played_by/2` facts are asserted (bidirectional)

#### AC-004-03: Link Role to Moment-Interval
- Given a role and a moment-interval exist
- When `link_role_to_mi/2` is called
- Then `role_participates_in/2` fact is asserted

#### AC-004-04: Add MI Detail
- Given a moment-interval exists
- When `add_mi_detail/4` is called with parent MI, detail ID, attributes, behaviours
- Then `mi_detail/3` fact is asserted linking detail to parent

#### AC-004-05: Link Plan to Actual
- Given two moment-intervals exist (plan and actual)
- When `link_plan_actual/3` is called
- Then `mi_plan_actual/3` fact is asserted with relationship type

### Test Mapping
- `test/test_schema.pl`: `link_role_to_player_creates_link`
- `test/test_schema.pl`: `link_role_to_mi_creates_participation`
- `test/test_schema.pl`: `add_mi_detail_creates_detail`
- `test/test_schema.pl`: `link_plan_actual_creates_link`

---

## REQ-005: Aggregate Boundaries

The system shall support defining DDD aggregates with roots, members, and invariants.

### Description

Aggregates are consistency boundaries containing a root entity and zero or more member entities. Invariants define business rules that must hold across the aggregate.

### Acceptance Criteria

#### AC-005-01: Define Aggregate with Root
- Given a context and a root object exist
- When `define_aggregate/3` is called with aggregate ID, context, root object
- Then `aggregate/2` and `aggregate_root/2` facts are asserted

#### AC-005-02: Add Aggregate Members
- Given an aggregate exists
- When `add_aggregate_member/2` is called with aggregate ID and member object
- Then `aggregate_member/2` fact is asserted

#### AC-005-03: Add Aggregate Invariant
- Given an aggregate exists
- When `add_aggregate_invariant/2` is called with aggregate ID and invariant description
- Then `aggregate_invariant/2` fact is asserted

#### AC-005-04: Query Aggregates in Context
- Given multiple aggregates defined in a context
- When `aggregates_in_context/2` is called
- Then all aggregates in that context are returned

### Test Mapping
- `test/test_schema.pl`: `define_aggregate_creates_aggregate`
- `test/test_schema.pl`: `add_aggregate_member_adds_member`
- `test/test_schema.pl`: `add_aggregate_invariant_adds_invariant`

---

## REQ-006: Domain Services

The system shall support defining domain services that coordinate operations across aggregates.

### Description

Domain services encapsulate operations that don't naturally belong to any single entity. They coordinate work across multiple aggregates.

### Acceptance Criteria

#### AC-006-01: Define Domain Service
- Given a valid context exists
- When `define_service/3` is called with ID, context, description
- Then `domain_service/2` fact is asserted

#### AC-006-02: Add Service Operation
- Given a domain service exists
- When `add_service_operation/4` is called with service ID, operation name, inputs, outputs
- Then `service_operation/3` fact is asserted

#### AC-006-03: Link Service to Coordinated Objects
- Given a domain service and domain objects exist
- When `link_service_to_object/2` is called
- Then `service_coordinates/2` fact is asserted

### Test Mapping
- `test/test_schema.pl`: `define_service_creates_service`
- `test/test_schema.pl`: `add_service_operation_adds_operation`

---

## REQ-007: Domain Classification

The system shall classify domains as Core, Supporting, or Generic.

### Description

- **Core Domain**: Competitive advantage, primary business differentiator
- **Supporting Domain**: Necessary but not differentiating
- **Generic Domain**: Off-the-shelf solutions, commoditized

### Acceptance Criteria

#### AC-007-01: Mark Core Domain
- Given a domain object or context exists
- When `mark_core_domain/1` is called
- Then `core_domain/1` fact is asserted

#### AC-007-02: Mark Supporting Domain
- Given a domain object or context exists
- When `mark_supporting_domain/1` is called
- Then `supporting_domain/1` fact is asserted

#### AC-007-03: Mark Generic Domain
- Given a domain object or context exists
- When `mark_generic_domain/1` is called
- Then `generic_domain/1` fact is asserted

#### AC-007-04: Mutual Exclusivity
- A domain object shall be classified in at most one category
- Classifying in a new category should replace the previous classification
- Implementation: `retract_domain_classification/1` removes existing before asserting new

### Test Mapping
- `test/test_requirements.pl`: `ac_007_04_mutual_exclusivity`

---

## REQ-008: Model Validation

The system shall validate models against DDD best practices and archetype rules.

### Description

Validation ensures structural integrity and catches common modeling errors such as orphaned objects, missing links, and archetype misclassification.

### Acceptance Criteria

#### AC-008-01: Validate Archetype Coverage
- Given a bounded context
- When `check_archetype_coverage/1` is called
- Then verification passes if all objects are classified into exactly one archetype

#### AC-008-02: Validate Link Consistency
- Given a bounded context
- When `check_link_consistency/1` is called
- Then verification passes if all links follow the Blue→Green→Yellow→Pink pattern

#### AC-008-03: Validate Role-Player Links
- Given a bounded context with roles
- When validation runs
- Then all roles must have at least one player (PPT) linked

#### AC-008-04: Validate MI Temporal Type
- Given moment-intervals in a context
- When validation runs
- Then all MIs must have temporal_type declared as `moment` or `interval`

#### AC-008-05: Generate Validation Report
- Given a bounded context
- When `generate_validation_report/2` is called
- Then a structured report is returned listing all validation results and warnings

#### AC-008-06: Detect Common Errors
- The system shall detect:
  - Moment-intervals incorrectly modeled as things
  - Missing role separation (party directly participating in MI)
  - Descriptions incorrectly modeled as things
  - Oversized aggregates (>7 members)
  - Roles without players

---

## REQ-009: Graphviz Visualization

The system shall generate Graphviz DOT code for visualizing domain models.

### Description

Visualization produces UML-style class diagrams with archetype color-coding, relationship arrows, and aggregate boundaries.

### Acceptance Criteria

#### AC-009-01: Generate Full Model DOT
- Given a populated domain model
- When `generate_dot/2` or `generate_dot/3` is called
- Then valid Graphviz DOT code is returned

#### AC-009-02: Archetype Color Coding
- Moment-Intervals shall be pink (#FFB6C1)
- Roles shall be yellow (#FFFF99)
- Party/Place/Things shall be green (#90EE90)
- Descriptions shall be blue (#ADD8E6)
- Services shall be plum (#DDA0DD)

#### AC-009-03: UML-Style Node Records
- Each node shall display:
  - Stereotype (e.g., `<<moment-interval>>`)
  - Object name
  - Attributes compartment (optional)
  - Behaviours compartment (optional)

#### AC-009-04: Relationship Visualization
- `describes` links: dashed arrows
- `plays` links: solid arrows
- `participates` links: solid arrows
- `contains` (MI→Detail): bold diamond arrows
- `plan/actual` links: dashed arrows with type label
- `coordinates` (service): dotted arrows

#### AC-009-05: Aggregate Subgraphs
- When `show_aggregates(true)` option is set
- Then aggregates are rendered as dashed, rounded subgraph clusters
- Containing root and member nodes

#### AC-009-06: Legend Generation
- Each diagram shall include a legend subgraph
- Showing archetype colors and their meanings

#### AC-009-07: Write DOT to File
- Given model and file path
- When `generate_dot_file/2` or `generate_dot_file/3` is called
- Then DOT code is written to the specified file

#### AC-009-08: Context-Specific Diagram
- Given a context ID
- When `generate_context_dot/3` is called
- Then DOT code is generated containing only objects from that context

#### AC-009-09: Context Map Diagram
- Given a model with multiple contexts
- When `generate_context_map_dot/2` is called
- Then a diagram is generated showing contexts as boxes and relationships with pattern-specific styles

#### AC-009-10: Render to Image Formats
- Given a DOT file and Graphviz installed
- When `render_to_png/2`, `render_to_svg/2`, or `render_to_pdf/2` is called
- Then the appropriate shell command is executed to render the image

---

## REQ-010: Model Export

The system shall export models in multiple formats for implementation in target languages.

### Description

The Prolog model serves as a canonical representation. Exports enable implementation in any language: TypeScript, Java, Python, Go, etc.

### Acceptance Criteria

#### AC-010-01: Export to Prolog Facts
- Given a populated model
- When `export_model_to_file/2` is called
- Then a Prolog file is written containing all model facts with documentation comments

#### AC-010-02: Export to Structured Term
- Given a populated model
- When `export_model_term/2` is called
- Then a nested Prolog term is returned representing the full model structure
- Format: `model(Name, Contexts, Objects, Relationships, Aggregates, Services, Classifications)`

#### AC-010-03: Export Context to Term
- Given a context ID
- When `export_context_term/2` is called
- Then a term is returned with all objects and relationships in that context

#### AC-010-04: Generate Text Report
- Given a populated model
- When `generate_model_report/2` is called
- Then a human-readable text report is generated summarizing the model

#### AC-010-05: Export to Workflowy Format
- Given a populated model
- When `export_to_workflowy/2` is called
- Then hierarchically indented text is generated suitable for Workflowy import

#### AC-010-06: Export to JSON (Future)
- Given a populated model
- When `export_to_json/2` is called
- Then a JSON document is generated with the complete model structure
- Status: **Not Implemented**

#### AC-010-07: Export to YAML (Future)
- Given a populated model
- When `export_to_yaml/2` is called
- Then a YAML document is generated with the complete model structure
- Status: **Not Implemented**

---

## REQ-011: Attributes and Behaviours

The system shall support defining attributes and behaviours for domain objects.

### Description

Attributes define the data properties of objects. Behaviours define operations that objects can perform.

### Acceptance Criteria

#### AC-011-01: Add Attribute to Object
- Given a domain object exists
- When `add_attribute/4` is called with object ID, name, type, required flag
- Then appropriate `*_attribute/3` fact is asserted based on archetype

#### AC-011-02: Add Behaviour to Object
- Given a domain object exists
- When `add_behaviour/4` is called with object ID, name, inputs, outputs
- Then appropriate `*_behaviour/3` fact is asserted based on archetype

#### AC-011-03: MI Status States
- Given a moment-interval exists
- When `add_mi_status_state/3` is called with MI ID, state name, transitions
- Then `mi_status/3` fact is asserted defining the state machine

### Test Mapping
- `test/test_schema.pl`: `add_mi_status_state_creates_status`

---

## REQ-012: Query Interface

The system shall provide query predicates for exploring the domain model.

### Description

Query helpers enable programmatic exploration of the model for analysis, reporting, and code generation.

### Acceptance Criteria

#### AC-012-01: Query All Objects in Context
- Given a context ID
- When `objects_in_context/2` is called
- Then a list of all object IDs in that context is returned

#### AC-012-02: Query by Archetype
- `moment_intervals_in_context/2` returns all MIs in a context
- `roles_in_context/2` returns all roles in a context
- `ppts_in_context/2` returns all PPTs in a context
- `descriptions_in_context/2` returns all descriptions in a context

#### AC-012-03: Query Aggregates and Services
- `aggregates_in_context/2` returns all aggregates in a context
- `services_in_context/2` returns all services in a context

### Test Mapping
- `test/test_schema.pl`: `objects_in_context_returns_all`
- `test/test_schema.pl`: `moment_intervals_in_context`

---

## Non-Functional Requirements

### NFR-001: Prolog Compatibility
- The system shall run on SWI-Prolog 8.0 or later
- All code shall follow the Covington et al. (2011) Prolog coding guidelines

### NFR-002: Steadfastness
- All predicates shall be steadfast: behaving correctly when output arguments are pre-instantiated

### NFR-003: Error Messages
- Error messages shall include:
  - Predicate name where error was detected
  - The offending data value
  - Explanation of what was expected

### NFR-004: Documentation
- All exported predicates shall have `%%` documentation comments
- Including mode specifiers (+, -, ?) and determinism (det, semidet, nondet)

### NFR-005: Extensibility
- The export system shall be designed for easy addition of new output formats
- New archetypes (if ever needed) should require minimal schema changes

---

## Implementation Status

| Requirement | Status | Coverage |
|-------------|--------|----------|
| REQ-001 | ✅ Complete | 4/4 AC |
| REQ-002 | ✅ Complete | 4/4 AC |
| REQ-003 | ✅ Complete | 5/5 AC |
| REQ-004 | ✅ Complete | 5/5 AC |
| REQ-005 | ✅ Complete | 4/4 AC |
| REQ-006 | ✅ Complete | 3/3 AC |
| REQ-007 | ✅ Complete | 4/4 AC |
| REQ-008 | ✅ Complete | 6/6 AC |
| REQ-009 | ✅ Complete | 10/10 AC |
| REQ-010 | ⚠️ Partial | 5/7 AC (JSON/YAML not implemented) |
| REQ-011 | ✅ Complete | 3/3 AC |
| REQ-012 | ✅ Complete | 3/3 AC |

---

## Test Coverage Summary

```
Unit Tests (test_schema.pl): 27 tests
  - bounded_context: 4 tests
  - moment_interval: 4 tests
  - role: 3 tests
  - party_place_thing: 3 tests
  - description: 2 tests
  - aggregate: 3 tests
  - service: 2 tests
  - archetype_classification: 4 tests
  - query_helpers: 2 tests

SDD Requirement Tests (test_requirements.pl): 41 tests
  - req_001_domain_input: 2 tests
  - req_002_bounded_context: 5 tests
  - req_003_archetypes: 9 tests
  - req_004_linking: 5 tests
  - req_005_aggregates: 4 tests
  - req_006_services: 3 tests
  - req_007_classification: 4 tests
  - req_009_visualization: 4 tests
  - req_011_attributes: 1 test
  - req_012_queries: 3 tests
  - integration: 1 test

Total: 68 tests
```

---

## Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-31 | Claude | Initial SDD specification |
