# model-step

A Prolog-based domain modeling toolkit implementing Domain-Driven Design (DDD) principles with four-archetype classification and Graphviz visualization.

## Overview

model-step transforms domain knowledge into structured models that can be:
- Validated against DDD best practices
- Visualized as UML-style class diagrams
- Exported to language-agnostic formats for implementation

The Prolog model serves as a canonical representation exportable to any target language (TypeScript, Java, Python, Go, etc.).

## Requirements

- **SWI-Prolog 8.0+** - Core runtime
- **Graphviz** (optional) - For rendering diagrams to PNG/SVG/PDF

```bash
# macOS
brew install swi-prolog graphviz

# Ubuntu/Debian
sudo apt install swi-prolog graphviz
```

## Quick Start

All commands assume you're in the `model-step` directory:

```bash
cd model-step
```

### 1. Load the System

```prolog
?- [src/larj_model].
```

### 2. Build a Model

```prolog
?- clear_model,
   define_context(order_ctx, 'Order Management', 'Handles customer orders'),
   define_moment_interval(order, order_ctx, moment, 'Customer purchase event'),
   define_role(purchaser, order_ctx, 'Buying context', 'Party placing the order'),
   define_party(customer, order_ctx, person, 'Individual customer'),
   link_role_to_player(purchaser, customer),
   link_role_to_mi(purchaser, order).
```

### 3. Validate the Model

```prolog
?- validate_full_model(Results), maplist(writeln, Results).
```

### 4. Generate Visualization

```prolog
?- generate_dot_file(my_model, 'output/model.dot', [show_attributes(true)]).
```

### 5. Render to Image

```bash
dot -Tpng output/model.dot -o output/model.png
```

## CLI Tool: Generate Models from Markdown

A Python CLI tool that uses Claude to automatically generate domain models from markdown descriptions.

### Setup

```bash
cd model-step/cli
pip install -r requirements.txt
```

Create a `.env` file in `model-step/` with your Anthropic API key (see `.env.example`):

```bash
ANTHROPIC_API_KEY=your-key-here
ANTHROPIC_MODEL=claude-opus-4-20250514
```

### Usage

```bash
# Generate model from markdown file
python cli/ddd_model.py domain_description.md

# With options
python cli/ddd_model.py domain.md --name my_model --verbose

# Output only the Prolog code (no files)
python cli/ddd_model.py domain.md --prolog-only

# Skip visualization
python cli/ddd_model.py domain.md --no-visualize
```

### Options

| Option | Description |
|--------|-------------|
| `-o, --output PATH` | Output Prolog file path |
| `-n, --name NAME` | Model name (default: from filename) |
| `-v, --visualize` | Generate Graphviz visualizations (default) |
| `-V, --no-visualize` | Skip visualization |
| `--validate/--no-validate` | Run model validation |
| `--verbose` | Show verbose output |
| `--prolog-only` | Only output Prolog code |

### Example

Given a markdown file `hotel_booking.md`:

```markdown
# Hotel Booking System

Guests make reservations for rooms. Each room has a room type
(single, double, suite) with standard rates. Reservations track
check-in and check-out dates. Guests can be individuals or companies.
```

Run:

```bash
python cli/ddd_model.py hotel_booking.md --verbose
```

Output:
- `output/hotel_booking_model.pl` - Generated Prolog model
- `output/hotel_booking.dot` - Graphviz DOT file
- `output/hotel_booking.png` - Model diagram
- `output/hotel_booking_context_map.dot` - Context map

## Running Tests

```bash
./scripts/run_tests.sh              # Run all tests
./scripts/run_tests.sh --visualize  # Also generate PNG/SVG
```

Or directly in Prolog:

```prolog
?- [test/test_schema], run_tests.
?- [test/test_requirements], run_tests.
```

## Four-Archetype Classification

All domain objects are classified into exactly one archetype:

| Archetype | Color | Purpose | Examples |
|-----------|-------|---------|----------|
| **Moment-Interval** | Pink | Temporal business events the system tracks | Order, Shipment, Reservation |
| **Role** | Yellow | How parties participate in events | Purchaser, Seller, Attendee |
| **Party/Place/Thing** | Green | Entities with unique identity | Customer, Product, Warehouse |
| **Description** | Blue | Catalog/template entries | ProductCategory, CustomerTier |

### Standard Link Pattern

Objects link in this directional pattern:

```
Description ──describes──▶ PPT ──plays──▶ Role ──participates──▶ Moment-Interval
    (Blue)                (Green)        (Yellow)                    (Pink)
                                                                        │
                                                                        ▼
                                                                    MI-Detail
```

## DSL Reference

### Bounded Contexts

```prolog
% Define a bounded context
define_context(ContextId, Name, Scope).

% Add context-specific terminology
add_context_term(ContextId, Term, Meaning).

% Link contexts with relationship patterns
link_contexts(Upstream, Downstream, Pattern, Notes).
% Pattern: shared_kernel | customer_supplier | conformist | anticorruption_layer | separate_ways
```

### Moment-Intervals (Pink)

```prolog
% Define a moment-interval
define_moment_interval(Id, ContextId, TemporalType, TrackedFor).
% TemporalType: moment | interval

% Add sub-detail
add_mi_detail(ParentMI, DetailId, Attributes, Behaviours).

% Define status lifecycle
add_mi_status_state(Id, StateName, TransitionsTo).

% Link plan to actual
link_plan_actual(PlanMI, ActualMI, RelationType).
```

### Roles (Yellow)

```prolog
% Define a role
define_role(Id, ContextId, RoleContext, Description).

% Link role to its player (PPT)
link_role_to_player(RoleId, PPTId).

% Link role to moment-interval participation
link_role_to_mi(RoleId, MIId).
```

### Party/Place/Thing (Green)

```prolog
% Define entities
define_party(Id, ContextId, PartyType, Description).   % person | organisation
define_place(Id, ContextId, PlaceType, Description).
define_thing(Id, ContextId, ThingType, Description).

% Link to description
link_ppt_to_description(PPTId, DescriptionId).

% Link to role
link_ppt_to_role(PPTId, RoleId).
```

### Descriptions (Blue)

```prolog
% Define a description/catalog entry
define_description(Id, ContextId, DescribesType, CatalogInfo).

% Add default values
add_description_default(Id, AttributeName, DefaultValue).
```

### Aggregates

```prolog
% Define aggregate with root
define_aggregate(AggId, ContextId, RootObjectId).

% Add members
add_aggregate_member(AggId, MemberObjectId).

% Add invariants
add_aggregate_invariant(AggId, InvariantDescription).
```

### Domain Services

```prolog
% Define a domain service
define_service(Id, ContextId, Description).

% Add operations
add_service_operation(ServiceId, OpName, Inputs, Outputs).

% Link to coordinated objects
link_service_to_object(ServiceId, ObjectId).
```

### Domain Classification

```prolog
mark_core_domain(Id).        % Competitive advantage
mark_supporting_domain(Id).  % Necessary but not differentiating
mark_generic_domain(Id).     % Off-the-shelf solutions
```

### Attributes and Behaviours

```prolog
add_attribute(ObjectId, Name, Type, Required).
add_behaviour(ObjectId, Name, Inputs, Outputs).
```

## Model Completeness Queries

Run these queries to check your model's completeness and correctness.

### Full Validation

```prolog
% Run all validation checks
?- validate_full_model(Results), maplist(writeln, Results).

% Validate a specific context
?- validate_context(order_ctx, Results), maplist(writeln, Results).

% Generate a validation report
?- generate_validation_report(my_model, Report), writeln(Report).
```

### Archetype Coverage

```prolog
% Verify all objects are classified into exactly one archetype
?- check_archetype_coverage(Details).

% Check all MIs have temporal type (moment/interval) specified
?- check_temporal_concepts(Details).

% Check roles are separated from their players
?- check_role_separation(Details).

% Check descriptions have applies_to links
?- check_description_extraction(Details).

% Verify the Blue→Green→Yellow→Pink link pattern
?- check_link_consistency(Details).
```

### Moment-Interval Checks

```prolog
% Verify all MIs have temporal nature
?- check_mi_temporal_nature(Details).

% Check MIs have status/lifecycle defined
?- check_mi_lifecycle(Details).

% List MIs and their details
?- check_mi_details(Details).

% Verify plan/actual relationships
?- check_mi_plan_actual(Details).
```

### Role Checks

```prolog
% Verify all roles have players
?- check_role_players(Details).

% Check roles have context information
?- check_role_context(Details).

% Verify roles participate in MIs
?- check_role_mi_links(Details).
```

### Party/Place/Thing Checks

```prolog
% Verify all PPTs have subtypes
?- check_ppt_subtype(Details).

% Check PPTs have identifier attributes
?- check_ppt_identity(Details).

% List PPT to Description links
?- check_ppt_description_links(Details).

% List PPTs and their roles
?- check_ppt_roles(Details).
```

### Description Checks

```prolog
% Check descriptions have applies_to targets
?- check_description_reuse(Details).

% Check descriptions have default values
?- check_description_defaults(Details).

% Verify description instance links
?- check_description_instance_links(Details).
```

### Aggregate Checks

```prolog
% Verify aggregates have roots
?- check_aggregate_boundaries(Details).

% Verify MI + Details are in same aggregate
?- check_mi_aggregate_cohesion(Details).

% List cross-aggregate references
?- check_cross_aggregate_refs(Details).

% List aggregate invariants
?- check_aggregate_invariants(Details).
```

### Link Pattern Checks

```prolog
% Check Description → PPT links
?- check_blue_green_links(Result).

% Check PPT → Role links
?- check_green_yellow_links(Result).

% Check Role → MI links
?- check_yellow_pink_links(Result).

% Check MI → Detail links
?- check_pink_detail_links(Result).
```

### Error Detection

```prolog
% Detect business events incorrectly modeled as things
?- detect_mi_as_thing(Issues), maplist(writeln, Issues).

% Detect PPTs participating directly in MIs without roles
?- detect_missing_role_separation(Issues), maplist(writeln, Issues).

% Detect catalog entries incorrectly tracked individually
?- detect_description_as_thing(Issues), maplist(writeln, Issues).

% Detect individual items incorrectly modeled as descriptions
?- detect_thing_as_description(Issues), maplist(writeln, Issues).

% Detect aggregates with multiple MIs
?- detect_oversized_aggregate(Issues), maplist(writeln, Issues).

% Detect roles without players
?- detect_role_without_player(Issues), maplist(writeln, Issues).
```

### Orphan and Unlinked Object Queries

```prolog
% List objects not in any aggregate
?- list_orphan_objects(Orphans), maplist(writeln, Orphans).

% List roles without players or MI participation
?- list_unlinked_roles(Roles), maplist(writeln, Roles).

% List MIs without role participation
?- list_mi_without_participants(MIs), maplist(writeln, MIs).

% List PPTs that don't play any roles
?- list_ppt_without_roles(PPTs), maplist(writeln, PPTs).
```

### Context and Object Queries

```prolog
% List all bounded contexts
?- findall(ctx(Id, Name), bounded_context(Id, Name, _), Contexts), maplist(writeln, Contexts).

% List all objects in a context
?- objects_in_context(order_ctx, Objects), maplist(writeln, Objects).

% List objects by archetype
?- moment_intervals_in_context(order_ctx, MIs), maplist(writeln, MIs).
?- roles_in_context(order_ctx, Roles), maplist(writeln, Roles).
?- ppts_in_context(order_ctx, PPTs), maplist(writeln, PPTs).
?- descriptions_in_context(order_ctx, Descs), maplist(writeln, Descs).

% List aggregates and services
?- aggregates_in_context(order_ctx, Aggs), maplist(writeln, Aggs).
?- services_in_context(order_ctx, Svcs), maplist(writeln, Svcs).

% Get archetype of any object
?- archetype_of(order, Archetype).
```

## Visualization

### Generate Full Model Diagram

```prolog
% Generate DOT code
?- generate_dot(my_model, DotCode), writeln(DotCode).

% Write to file
?- generate_dot_file(my_model, 'output/model.dot', []).

% With options
?- generate_dot_file(my_model, 'output/model.dot', [
       show_attributes(true),
       show_behaviours(true),
       show_aggregates(true),
       rankdir('LR')
   ]).
```

### Generate Context-Specific Diagram

```prolog
?- generate_context_dot(order_ctx, my_model, DotCode).
?- generate_context_dot_file(order_ctx, my_model, 'output/order_ctx.dot').
```

### Generate Context Map

```prolog
?- generate_context_map_dot(my_model, DotCode).
```

### Render to Images

```prolog
?- render_to_png('output/model.dot', 'output/model.png').
?- render_to_svg('output/model.dot', 'output/model.svg').
?- render_to_pdf('output/model.dot', 'output/model.pdf').
```

Or from command line:

```bash
dot -Tpng output/model.dot -o output/model.png
dot -Tsvg output/model.dot -o output/model.svg
```

## Export Formats

### Export to Prolog Facts

```prolog
?- export_model_to_file(my_model, 'output/model_export.pl').
?- export_context_to_file(order_ctx, my_model, 'output/order_ctx.pl').
```

### Export to Structured Term

```prolog
?- export_model_term(my_model, Term), writeln(Term).
?- export_context_term(order_ctx, Term), writeln(Term).
```

### Generate Text Report

```prolog
?- generate_model_report(my_model, Report), writeln(Report).
?- generate_context_report(order_ctx, Report), writeln(Report).
```

### Export to Workflowy Format

```prolog
?- export_to_workflowy(my_model, Text), writeln(Text).
```

## Example: E-Commerce Domain

See `examples/ecommerce_model.pl` for a complete example with:
- 3 bounded contexts (Order, Inventory, Customer)
- Moment-intervals: Order, Shipment, StockReservation
- Roles: Purchaser, StockItem, AccountHolder
- Entities: Customer, Product
- Descriptions: ProductCategory, CustomerTier
- Aggregates with invariants
- Domain services

```prolog
?- [examples/ecommerce_model].
?- build_ecommerce_model.
?- example_queries.
```

## Project Structure

```
model-step/
├── cli/                    # Python CLI tool
│   ├── ddd_model.py        # Main CLI entry point
│   ├── prompts.py          # Claude prompt templates
│   ├── prolog_runner.py    # Prolog interface
│   └── requirements.txt    # Python dependencies
├── src/
│   ├── ddd_schema.pl       # Core schema (40+ predicates)
│   ├── model_builder.pl    # High-level DSL
│   ├── larj_model.pl       # Main entry point
│   ├── model_visualize.pl  # Graphviz generation
│   ├── model_export.pl     # Export utilities
│   └── model_validate.pl   # Validation framework
├── examples/
│   └── ecommerce_model.pl  # Complete example
├── test/
│   ├── test_schema.pl      # Unit tests
│   └── test_requirements.pl # SDD requirement tests
├── scripts/
│   └── run_tests.sh        # Test runner
├── output/                 # Generated artifacts
├── .env                    # API keys (git-ignored)
├── .env.example            # Environment template
├── README.md               # This file
├── SPEC.md                 # SDD specification
├── DDD.md                  # DDD methodology guide
├── modelling.md            # Domain modeling instructions
└── CLAUDE.md               # Prolog coding standards
```

## Documentation

- **SPEC.md** - Formal specification with requirements and acceptance criteria
- **DDD.md** - DDD processing methodology and archetype decision tree
- **modelling.md** - Domain model design guidelines
- **CLAUDE.md** - Prolog coding standards (Covington et al., 2011)

## License

MIT
