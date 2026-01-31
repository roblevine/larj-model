# Domain-Driven Design: Model Processing Instructions

## Purpose

Process domain knowledge artifacts to produce a structured domain model following DDD principles. Focus on systematic classification using archetype analysis, boundary definition, and quality validation.

---

## Input Processing

### Accepted Inputs

- Domain expert transcripts and interview notes
- Ubiquitous language glossaries
- Business process descriptions
- Existing documentation (rules, policies, workflows)
- Initial concept sketches or diagrams

---

## Processing Steps

### Step 1: Extract Domain Concepts

Scan all inputs and list every noun representing a business concept. For each concept, record:

- Name (using exact terminology from input)
- Description (how domain experts define it)
- Relationships (what other concepts it connects to)
- Behaviours (what actions involve this concept)

### Step 2: Identify Bounded Contexts

Look for boundaries where:

| Signal | Action |
|--------|--------|
| Same term has different meanings | Split into separate contexts |
| Different teams own different areas | Mark as separate contexts |
| Different business capabilities described | Consider separate contexts |
| Strong transactional coupling exists | Keep in same context |

**Output for each context:**
- Name
- Scope (what it owns)
- Key terms with context-specific meanings

### Step 3: Map Context Relationships

For each pair of contexts that interact, assign one relationship pattern:

| Pattern | When to Apply |
|---------|---------------|
| **Shared Kernel** | Teams coordinate closely, share model subset |
| **Customer/Supplier** | Upstream serves downstream; cooperative |
| **Conformist** | Downstream adopts upstream model; no negotiation |
| **Anti-Corruption Layer** | Integration with legacy or mismatched models |
| **Separate Ways** | No integration needed |

---

### Step 4: Classify Domain Objects Using Archetypes

Domain concepts are classified using four interconnected archetypes that form a domain-neutral component. This approach provides systematic guidance for identifying classes and their responsibilities.

#### The Four Archetypes

##### 1. Moment-Interval (Pink) — Highest Priority

A moment in time or interval of time that the system needs to track for business or legal reasons.

**Identification questions:**
- Is this something that happens at a specific moment or over an interval?
- Does the business need to track or act upon this event?
- Does it have business or legal significance?

**Typical attributes:**
- Number/identifier
- Date, datetime, or interval (start/end)
- Priority
- Total (calculated value)
- Status

**Typical behaviours:**
- `make[MomentInterval]` — business process for creation
- `addDetail` — add component parts
- `calcTotal` / `recalcTotal` — compute totals
- `complete` / `cancel` — lifecycle transitions
- `generateNext` — create subsequent moment-interval
- `assessWRTPrior` / `assessWRTNext` — evaluate against related moments

**Examples:**
- Sale, Order, Reservation, Booking
- Payment, Invoice, Shipment
- Rental (interval from checkout to return)
- Claim, Application, Request

**Moment-Interval Details:**
Moment-intervals often have parts called details. These are components that belong to the moment-interval and contribute to its totals.

| Detail | Parent Moment-Interval |
|--------|------------------------|
| OrderLine | Order |
| InvoiceItem | Invoice |
| ReservationItem | Reservation |

**Detail attributes:** quantity, calculated total

**Plan vs Actual Pattern:**
Moment-intervals frequently come in plan/actual pairs:
- PlannedProcess → ActualProcess
- ScheduledDelivery → ActualDelivery
- Quote → Order

##### 2. Role (Yellow) — Second Priority

A way of participation by a party, place, or thing. Roles represent the "hat" that an entity wears in a particular context.

**Identification questions:**
- Is this a way that a party/place/thing participates in the domain?
- Can the same underlying entity play multiple roles?
- Does this represent context-specific behaviour or status?

**Typical attributes:**
- Assigned number
- Status (active, suspended, etc.)

**Typical behaviours:**
- `assessAcrossMIs` — evaluate across moment-intervals
- `listMIs` — list associated moment-intervals
- `assessAcrossRoles` — evaluate across related roles

**Examples:**
- Customer, Employee, Supplier (roles of Person/Organisation)
- Cashier, Manager, Owner (roles of Person)
- Warehouse, RetailOutlet (roles of Place)
- ProductForSale, ProductInManufacturing (roles of Thing)

**Role vs Party/Place/Thing:**
The role player (party/place/thing) captures core attributes that apply regardless of what roles are being played. The role captures context-specific attributes and behaviours.

| Role Player (Green) | Role (Yellow) |
|---------------------|---------------|
| Person | Employee, Customer, Supplier |
| Organisation | Vendor, Partner, Subsidiary |
| Location | Warehouse, Store, ServicePoint |
| Vehicle | RentalAsset, MaintenanceSubject |

##### 3. Party, Place, or Thing (Green) — Third Priority

Someone or something that plays roles and participates in moment-intervals.

**Subtypes:**
- **Party:** Person or Organisation
- **Place:** Physical or logical location
- **Thing:** Physical object or asset

**Identification questions:**
- Is this someone or something with independent existence?
- Does it have a unique identity (serial number, identifier)?
- Can it play different roles in different contexts?
- Is it tracked individually (not just by quantity)?

**Typical attributes:**
- Serial number / identifier
- Name
- Address / location
- Custom values (specific to this instance)

**Typical behaviours:**
- `assessAcrossRoles` — evaluate across roles played
- `getCustomElseDefaultValue` — get instance value or fall back to description default
- `listRoles` — list roles currently played

**Examples:**
- Person, Organisation, Company
- Store, Warehouse, Region
- Vehicle, Equipment, Product (individual items)

##### 4. Description (Blue) — Fourth Priority (Default for remaining concepts)

A catalog-entry-like description providing values that apply again and again. Descriptions define the template; parties/places/things are the instances.

**Identification questions:**
- Is this a grouping of values that applies to multiple instances?
- Does it define defaults or standard values?
- Is it like a catalog entry or product specification?

**Typical attributes:**
- Type / category
- Description text
- Item number / SKU
- Default values

**Typical behaviours:**
- `assessAcrossPPTs` — evaluate across corresponding parties/places/things
- `findAvailable` — find available instances
- `calcQtyAvailable` — calculate quantity available
- `calcTotalFor` — calculate total for a detail line

**Examples:**
- ProductDescription, VehicleDescription
- ServiceType, AccountType
- RoomCategory, EquipmentClass

#### Archetype Classification Sequence

Apply this ordered checklist to each concept:
```
1. Is it a moment in time or interval of time that needs tracking?
   → YES: Pink Moment-Interval
   → NO: Continue to 2

2. Is it a role played by a party, place, or thing?
   → YES: Yellow Role
   → NO: Continue to 3

3. Is it a catalog-entry-like description with reusable values?
   → YES: Blue Description
   → NO: Continue to 4

4. Default: It's a party, place, or thing
   → Green Party/Place/Thing
```

#### Standard Archetype Links

Archetypes connect in predictable patterns:
```
Description (Blue)
     │
     │ describes
     ▼
Party/Place/Thing (Green)
     │
     │ plays
     ▼
Role (Yellow)
     │
     │ participates in
     ▼
Moment-Interval (Pink)
     │
     │ contains
     ▼
MI-Detail (Pink)
```

**Link semantics:**

| From | To | Relationship |
|------|----|--------------|
| Description | Party/Place/Thing | One description applies to many instances |
| Party/Place/Thing | Role | One entity plays many roles |
| Role | Moment-Interval | One role participates in many moments |
| Moment-Interval | MI-Detail | One moment contains many details |
| Moment-Interval | Moment-Interval | Plan → Actual, Prior → Next |

#### Mapping Archetypes to DDD Building Blocks

| Archetype | DDD Classification | Aggregate Role |
|-----------|-------------------|----------------|
| Moment-Interval | Entity | Often Aggregate Root |
| MI-Detail | Entity (local identity) | Internal to Aggregate |
| Role | Entity | Internal or Aggregate Root |
| Party/Place/Thing | Entity | Often Aggregate Root |
| Description | Value Object or Entity | Depends on mutability needs |

**Key distinctions:**

- **Moment-Intervals** are almost always Entities (identity matters, lifecycle exists)
- **Descriptions** are typically Value Objects when immutable, Entities when catalog must be managed
- **Roles** bridge Party/Place/Thing to Moment-Interval; often internal to an Aggregate
- **Party/Place/Thing** are Entities with global identity

#### Services in the Archetype Model

Services emerge when behaviour doesn't belong to any single archetype instance:

| Service Type | Description |
|--------------|-------------|
| Process orchestration | Coordinates multiple moment-intervals |
| Cross-aggregate calculation | Computes values across aggregates |
| External system integration | Translates to/from external models |
| Policy enforcement | Applies rules across multiple objects |

#### Output Template for Step 4
```
## Domain Objects by Archetype

### Moment-Intervals (Pink)

#### [Name]
**Archetype:** moment-interval
**Temporal nature:** [moment | interval]
**Tracked for:** [business reason | legal reason]

**Attributes:**
- number: [type]
- dateTime/interval: [type]
- status: [enumeration of states]
- total: [calculated value if applicable]

**Behaviours:**
- make[Name]: [creation process description]
- complete/cancel: [lifecycle transitions]
- calcTotal: [calculation logic]

**Details (MI-Details):**
- [DetailName]: qty, calcTotal

**Plan/Actual Relationship:** [if applicable]

**Links:**
- Participated in by: [Role names]
- Prior/Next: [Related moment-intervals]

---

### Roles (Yellow)

#### [Name]
**Archetype:** role
**Role of:** [Party/Place/Thing name]
**Context:** [What context this role operates in]

**Attributes:**
- assignedNumber: [type]
- status: [enumeration]

**Behaviours:**
- assessAcrossMIs: [evaluation logic]

**Links:**
- Played by: [Party/Place/Thing]
- Participates in: [Moment-Interval names]

---

### Parties, Places, and Things (Green)

#### [Name]
**Archetype:** [party | place | thing]
**Subtype:** [person | organisation | location | physical object]

**Attributes:**
- identifier: [type]
- name: [type]
- [custom attributes]

**Behaviours:**
- assessAcrossRoles: [evaluation logic]
- getCustomElseDefaultValue: [fallback logic]

**Links:**
- Described by: [Description name]
- Plays roles: [Role names]

---

### Descriptions (Blue)

#### [Name]
**Archetype:** description
**Describes:** [Party/Place/Thing name]

**Attributes:**
- type: [categorisation]
- itemNumber: [catalog identifier]
- defaultValues: [list of defaults]

**Behaviours:**
- findAvailable: [availability logic]
- calcQtyAvailable: [quantity calculation]

**Links:**
- Applies to: [Party/Place/Thing instances]

---

### Domain Services

#### [Name]
**Operation:** [verb phrase]
**Coordinates:** [list of archetypes involved]
**Inputs:** [parameters]
**Outputs:** [return values]
**Rules:** [business logic]
```

---

### Step 5: Design Aggregates

Use archetype analysis to inform Aggregate boundaries.

**Aggregate patterns by archetype:**

| Pattern | Structure |
|---------|-----------|
| Moment-Interval Aggregate | Moment-Interval (root) + MI-Details |
| Party Aggregate | Party (root) + Roles |
| Thing Aggregate | Thing (root) + Roles |
| Description Aggregate | Description (root) — often standalone |

**Boundary rules:**

1. Moment-Interval + its Details form natural Aggregates
2. Party/Place/Thing may include Roles when lifecycle is coupled
3. Roles may be separate Aggregates when independently managed
4. Descriptions are often separate (referenced by ID)

**Cross-Aggregate references:**

- Moment-Intervals reference Roles by ID
- Roles reference Party/Place/Thing by ID
- Party/Place/Thing references Description by ID

### Step 6: Define Repositories and Factories

**Repository:** Create one per Aggregate root.

**Factory patterns by archetype:**

| Archetype | Factory Needs |
|-----------|---------------|
| Moment-Interval | Often needs Factory (complex creation with details, validation) |
| Party/Place/Thing | Factory if creation involves role setup |
| Description | Usually simple construction |

### Step 7: Identify Core Domain

Classify each area:

| Classification | Criteria |
|----------------|----------|
| **Core** | Differentiates the business; competitive advantage |
| **Supporting** | Necessary but not differentiating |
| **Generic** | Standard problem; use off-the-shelf solutions |

**Archetype guidance for Core Domain:**
- Moment-Intervals often represent Core Domain (business transactions)
- Descriptions are often Generic (catalog management)
- Party/Place/Thing may be Supporting (standard entity management)

---

## Output Structure
```
# Domain Model: [Name]

## Bounded Contexts
[List with scope and key terms]

## Context Map
[Relationships between contexts]

## Core Domain
[Vision statement and core concepts]

## Domain Objects by Archetype

### Moment-Intervals (Pink)
[Moments/intervals with attributes, behaviours, details]

### Roles (Yellow)
[Roles with participation context]

### Parties, Places, Things (Green)
[Entities with identity and role links]

### Descriptions (Blue)
[Catalog entries with default values]

### Domain Services
[Stateless operations]

## Aggregates
[Root, boundary contents, invariants]

## Repositories
[One per aggregate root]

## Factories
[Complex creation logic]

## Open Questions
[Ambiguities requiring domain expert input]
```

---

## Quality Validation

### Archetype Classification Validation

| Check | Pass Criteria |
|-------|---------------|
| Temporal concepts identified | All business events classified as Moment-Intervals |
| Role separation | Roles distinguished from their players |
| Description extraction | Reusable catalog values separated from instances |
| Default assignment | Remaining concepts assigned to Party/Place/Thing |
| Link consistency | Archetype links follow Blue→Green→Yellow→Pink pattern |

### Moment-Interval Validation

| Check | Pass Criteria |
|-------|---------------|
| Temporal nature stated | Moment or interval explicitly identified |
| Business justification | Reason for tracking documented |
| Lifecycle defined | Status states and transitions documented |
| Details identified | Component parts (MI-Details) listed if present |
| Plan/Actual paired | Related planned/actual moments linked |

### Role Validation

| Check | Pass Criteria |
|-------|---------------|
| Player identified | Associated Party/Place/Thing documented |
| Context specified | What context this role operates in |
| Distinction justified | Why role is separate from player |
| Moment-Interval links | What moments this role participates in |

### Party/Place/Thing Validation

| Check | Pass Criteria |
|-------|---------------|
| Subtype assigned | Classified as party, place, or thing |
| Identity defined | Unique identifier mechanism specified |
| Description linked | Associated catalog description if applicable |
| Roles listed | All roles this entity can play |

### Description Validation

| Check | Pass Criteria |
|-------|---------------|
| Reuse confirmed | Values apply to multiple instances |
| Defaults specified | Default values documented |
| Instance link | Associated Party/Place/Thing type identified |
| Mutability assessed | Classified as Value Object or managed Entity |

### Aggregate Boundary Validation

| Check | Pass Criteria |
|-------|---------------|
| Archetype alignment | Boundaries follow archetype patterns |
| Moment-Interval cohesion | MI + Details in same Aggregate |
| Cross-reference by ID | References across Aggregates use identity only |
| Invariants internal | Consistency rules within boundaries |

### Link Pattern Validation

| Check | Pass Criteria |
|-------|---------------|
| Description → PPT | Descriptions link to instances they describe |
| PPT → Role | Parties/places/things link to roles played |
| Role → MI | Roles link to moment-intervals participated in |
| MI → Detail | Moment-intervals contain their details |
| No reverse ownership | Details don't own their parent moment-intervals |

---

## Common Errors to Avoid

| Error | Symptom | Correction |
|-------|---------|------------|
| Moment-Interval as Thing | Business event modeled without temporal attributes | Add date/interval, status, consider lifecycle |
| Missing Role separation | Party directly participates in Moment-Interval | Insert Role to capture participation context |
| Description as Thing | Catalog entry tracked individually | Remove identity; make Value Object |
| Thing as Description | Individual item lacks identity | Add serial number; track individually |
| Oversized Aggregate | Multiple Moment-Intervals in boundary | Split; reference by ID across Aggregates |
| Missing MI-Details | Line items modeled as separate Aggregates | Include as details within Moment-Interval |
| Role without Player | Role exists without Party/Place/Thing | Identify the entity playing the role |

---

## Validation Summary Template
```
## Model Validation Results

### Archetype Coverage
- [ ] All temporal concepts → Moment-Intervals
- [ ] All participation patterns → Roles
- [ ] All catalog entries → Descriptions
- [ ] All participants → Party/Place/Thing
- [ ] Link pattern Blue→Green→Yellow→Pink verified

### Moment-Interval Quality
- [ ] Temporal nature documented
- [ ] Lifecycle states defined
- [ ] Details identified
- [ ] Plan/Actual relationships mapped

### Role Quality
- [ ] Players identified
- [ ] Contexts specified
- [ ] MI participation documented

### Aggregate Quality
- [ ] Boundaries follow archetype patterns
- [ ] MI + Details cohesive
- [ ] Cross-references by ID only

### Issues Found
[List any validation failures with remediation]

### Open Questions
[Ambiguities requiring domain expert clarification]
```