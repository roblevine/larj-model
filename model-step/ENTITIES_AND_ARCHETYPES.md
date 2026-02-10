# Entities, Aggregates, and Archetypes: A Foundation Guide

This guide explains the core concepts used in domain-driven modeling and how they interconnect to form a complete domain model.

---

## Core Concepts

### Entities

**An entity is any object with continuous identity through time.**

Entities represent things in your domain that:
- Have a unique identifier (e.g., order ID, customer ID, invoice number)
- Are tracked over their lifetime
- Can change attributes but maintain the same identity
- Are meaningful on their own or as part of a larger structure

**Examples:**
- A Customer (ID: C001) remains the same customer whether they have 1 order or 100
- An Order (ID: ORD-2024-001) is still that order even after payment is received
- A Device (MAC address) maintains identity across registrations and deregistrations

Entities are the "nouns" of your domain that matter independently.

---

### Aggregates

**An aggregate is a cluster of associated entities and value objects that form a consistency boundary.**

Aggregates:
- Group related entities together under a single root entity (the aggregate root)
- Define a unit of consistency—changes within an aggregate are atomic
- Have clear boundaries—external objects reference only the aggregate root
- Simplify transaction management by keeping related data together

**How aggregates work:**
1. **Aggregate Root**: One entity serves as the entry point (e.g., Order)
2. **Internal Objects**: Child entities and values belong inside (e.g., OrderLineItem, OrderAddress)
3. **Boundary**: Only the root is referenced from outside
4. **Consistency**: All changes within an aggregate succeed or fail together

**Example: An Order Aggregate**
```
Order (Aggregate Root)
├── OrderLineItems (child entities)
│   ├── Product reference
│   └── Quantity, Price
├── ShippingAddress (value object)
└── PaymentInfo (value object)
```

External code references the Order, never a LineItem directly. Changes to line items and shipping address succeed or fail as one unit.

---

### Bounded Contexts

**A bounded context is an explicit boundary within which a domain model is defined and applicable.**

Bounded contexts:
- Define where a model applies and where it doesn't
- Allow the same concept to have different meanings in different contexts
- Reduce cognitive load by limiting scope
- Enable parallel development by different teams

**Key signals for identifying boundaries:**

| Signal | Meaning |
|--------|---------|
| Same term, different meanings | Different contexts |
| Different teams own it | Different contexts |
| Different business processes | Consider splitting |
| Strong transactional coupling | Same context |
| Data is managed independently | Different contexts |

**Example: "Order" in different bounded contexts**
- **Order Management context**: Order = a customer's purchase request with items and quantities
- **Shipping context**: Order = a shipment tracking unit with destination and carrier
- **Billing context**: Order = an invoice with line items and payment terms

Each context has its own model of "Order" optimized for its purpose.

---

## The Four Archetypes

These archetypes classify domain concepts and guide structural decisions. They form the building blocks of your model.

### 1. Moment-Interval (Pink)

**Tracked moments or intervals in time that matter for business reasons.**

**Characteristics:**
- Represents an event or time span (e.g., a moment in time or an interval)
- Has a timestamp or date range
- Often has a status (pending, completed, cancelled)
- Contains line items or details that contribute to the whole
- Examples: Order, Payment, Session, Booking, Reservation, Shipping

**Why it matters:**
- Tracks business events needed for compliance, accounting, or operations
- Creates an audit trail
- Often generates detail records (e.g., an Order generates OrderLineItems)

**Real-world pattern—Plan/Actual:**
- Plan: Forecast, Budget, Planned Shipment
- Actual: Actual Order, Actual Payment, Actual Shipment

---

### 2. Role (Yellow)

**The way a party, place, or thing participates or acts in a specific context.**

**Characteristics:**
- Describes how something is used or what capacity it fills
- Same entity can have multiple roles
- Has limited identity (often just status, assignment details)
- Meaningful only in the context of a specific bounded context
- Examples: Purchaser, Account Holder, Registered Device, Employee, Manager

**Why it matters:**
- Separates "what something is" from "how it participates"
- One person can be a Purchaser, Employee, and Manager—each a different role
- Avoids forcing one entity into multiple unrelated responsibilities

**Pattern:**
```
Person (entity) plays → Customer (role) in Sales context
Person (entity) plays → Employee (role) in HR context
Device (entity) plays → Registered Device (role) in Access Control context
```

---

### 3. Description (Blue)

**A catalog entry that defines defaults, templates, or specifications for creating instances.**

**Characteristics:**
- Sets standard attributes and rules for a category of things
- Defines defaults (colors, sizes, prices, rules)
- Has limited identity (catalog ID, name)
- Linked to by roles or when creating individual instances
- Examples: Product Catalog, Device Type, Business Rules Configuration, Service Plan

**Why it matters:**
- Avoids duplicating rules across many individual objects
- Centralizes templates that roles and moment-intervals use
- Simplifies changes (update the template, behavior changes everywhere)

**Pattern:**
```
Product (Description) → defines → OrderLineItem (use in Moment-Interval)
Device Type (Description) → classifies → Device (Party)
Account Tier (Description) → assigned to → Account (Role)
```

---

### 4. Party / Place / Thing (Green)

**Entities with independent identity that exist in their own right and can play multiple roles.**

**Characteristics:**
- Has a unique, persistent identity
- Can exist independently
- Can play multiple different roles in different contexts
- Examples: Customer, Person, Company, Location, Device, Equipment, Building

**Why it matters:**
- Captures core entities of your domain
- Serves as the "hub" that roles refer to
- Maintains identity across different contexts and roles

**Pattern:**
```
Customer (Party) can be:
- Purchaser → Place orders
- Account Holder → Has accounts
- Payer → Makes payments
```

---

## How They Relate to Each Other

### The Model Hierarchy

```
Bounded Context
├── Aggregate Root (Entity)
│   ├── Child Entities (Archetype: Party/Place/Thing, Role, or Moment-Interval)
│   └── Value Objects (non-archetype attributes)
└── Other Entities within boundary
```

### Archetype Relationships

```
Description (Blue)
    ↓ defines/templates
    ├→ Role (Yellow) — "who participates how?"
    └→ Moment-Interval (Pink) — "what details should this track?"

Party/Place/Thing (Green)
    ↓ plays
    └→ Role (Yellow) — "how do they participate?"

Role (Yellow)
    ↓ involved in
    └→ Moment-Interval (Pink) — "what roles participate in this event?"

Moment-Interval (Pink)
    ↓ records/involves
    ├→ Role (Yellow) — "who was involved?"
    └→ Description (Blue) — "what rules apply?"
```

### Complete Example: E-Commerce Order

**Bounded Context:** Order Management

**Aggregate: Order**
```
Order (Moment-Interval)
├── Customer → Role played by Party (Green)
├── OrderLineItem (child Moment-Interval)
│   └── Product → Reference to Description (Blue)
└── ShippingAddress → Role instance
```

**Relationships:**
1. **Order** (Moment-Interval) - "A purchase event in time"
2. **Customer** (Role) - "How a Party participates as the buyer"
3. **Purchaser** (Party/Place/Thing) - "The actual person making the purchase"
4. **Product** (Description) - "Template defining name, price, availability"
5. **OrderLineItem** (child of Moment-Interval) - "What was ordered and how much"

---

## Design Rules

### When to Use Each Archetype

```
Question: "Is this a tracked moment or time interval?"
  Yes → Moment-Interval ✓
  No  ↓

Question: "Is this how something PARTICIPATES in this context?"
  Yes → Role ✓
  No  ↓

Question: "Is this a catalog, template, or defaults definition?"
  Yes → Description ✓
  No  ↓

Otherwise → Party/Place/Thing ✓
```

### Aggregate Design Principles

1. **Keep aggregates small** — Group only entities that must change together
2. **Reference by ID** — Other aggregates reference only the root by ID
3. **Enforce consistency** — All invariants within an aggregate must hold
4. **Clear patterns** — Use owner-child relationships (Order contains LineItems)
5. **Context boundaries** — An aggregate exists within one bounded context

### Bounded Context Boundaries

**Create a separate context when:**
- The same term means different things
- Different teams would own the model
- Different persistence or performance requirements exist
- Different update cycles or consistency rules apply

**Keep in the same context when:**
- Strong transactional coupling exists
- Shared ubiquitous language applies
- Single team has clear ownership

---

## Summary Table

| Concept | Purpose | Identity | Scope | Example |
|---------|---------|----------|-------|---------|
| **Entity** | Has continuous identity | Unique ID | Lifetime | Customer C001 |
| **Aggregate** | Consistency boundary | Root ID | Transactions | Order with LineItems |
| **Bounded Context** | Model boundary | N/A | Semantic | Order Management |
| **Moment-Interval** | Time-tracked events | Event ID | Within context | Order placed |
| **Role** | How something participates | Role ID | One context | Customer role |
| **Description** | Templates/catalogs | Catalog ID | Definitions | Product catalog |
| **Party/Place/Thing** | Independent entities | Entity ID | Multiple roles | Person, Location |

---

## Next Steps

- Read [DDD.md](DDD.md) for processing steps to extract these concepts from domain knowledge
- Read [modelling.md](modelling.md) for detailed archetype classification rules
- Review [examples/](examples/) for real domain models showing these patterns
- Check test cases in [test/](test/) to see validation rules for proper model structure
