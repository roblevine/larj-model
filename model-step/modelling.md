# Domain Model Design Instructions

Transform domain knowledge into a structured model using archetype-based classification.

---

## 1. Bounded Contexts

Identify context boundaries using these signals:

| Signal | Action |
|--------|--------|
| Same term, different meanings | Split contexts |
| Different teams/capabilities | Separate contexts |
| Strong transactional coupling | Keep together |

For each context: **Name**, **Scope**, **Key terms**

Context relationships: Shared Kernel | Customer/Supplier | Conformist | Anti-Corruption Layer | Separate Ways

---

## 2. Archetype Classification

Classify every domain concept using this decision sequence:

```
1. Tracked moment/interval in time? → MOMENT-INTERVAL (Pink)
2. Way something participates?      → ROLE (Yellow)
3. Catalog entry with defaults?     → DESCRIPTION (Blue)
4. Otherwise                        → PARTY/PLACE/THING (Green)
```

### Moment-Interval (Pink)
Business events tracked for legal/operational reasons.

- **Attributes:** id, timestamp/interval, status, totals
- **Contains:** MI-Details (line items contributing to parent)
- **Patterns:** Plan→Actual pairs, Prior→Next chains
- **Examples:** Order, Payment, Session, Booking, Alert

### Role (Yellow)
How a party/place/thing participates in a context.

- **Attributes:** assigned id, status
- **Key insight:** Same entity plays different roles (Person→Customer, Employee)
- **Examples:** AccountHolder, Payer, RegisteredDevice

### Party/Place/Thing (Green)
Entities with independent identity that play roles.

- **Subtypes:** Party (person/org), Place (location), Thing (asset)
- **Attributes:** identifier, name, instance-specific values
- **Examples:** Customer, Account, Device, Warehouse

### Description (Blue)
Catalog entries defining defaults for multiple instances.

- **Attributes:** type code, defaults, specifications
- **Examples:** AccountType, ProductCategory, ServiceTier

---

## 3. Standard Link Pattern

```
Description ──describes──▶ Party/Place/Thing ──plays──▶ Role ──participates──▶ Moment-Interval
                                                                                      │
                                                                                      ▼
                                                                                  MI-Detail
```

All links are one-to-many in the direction shown.

---

## 4. Aggregates

| Pattern | Structure |
|---------|-----------|
| Moment-Interval Aggregate | MI (root) + MI-Details |
| Entity Aggregate | Party/Place/Thing (root) + tightly-coupled Roles |
| Description | Usually standalone, referenced by ID |

**Rules:**
- Cross-aggregate references use IDs only
- MI + Details always stay together
- Roles can be separate aggregates if independently managed

---

## 5. Domain Services

Create services when behaviour spans multiple aggregates:
- Process orchestration (coordinates multiple MIs)
- Cross-aggregate calculations
- External integrations
- Policy enforcement

---

## 6. Core Domain Classification

| Type | Criteria |
|------|----------|
| **Core** | Competitive advantage; custom build |
| **Supporting** | Necessary but not differentiating |
| **Generic** | Standard problem; buy/reuse |

Moment-Intervals usually indicate Core Domain.

---

## Output Structure

```
# Domain Model: [Name]

## Bounded Contexts
[Name, scope, key terms for each]

## Context Map
[Relationships between contexts]

## Domain Objects

### Moment-Intervals (Pink)
[Name, temporal type, status states, details, links]

### Roles (Yellow)
[Name, played by, participates in]

### Parties/Places/Things (Green)
[Name, subtype, roles played]

### Descriptions (Blue)
[Name, describes, defaults]

### Services
[Name, coordinates, rules]

## Aggregates
[Root + contents for each]

## Open Questions
[Ambiguities for domain expert review]
```

---

## Common Errors

| Error | Fix |
|-------|-----|
| Event without temporal attributes | Add timestamp, status → make MI |
| Party directly in MI | Insert Role between them |
| Catalog entry with identity | Remove identity → Description |
| Individual item without identity | Add identifier → Thing |
| Multiple MIs in one aggregate | Split; reference by ID |
| Role without player | Identify the Party/Place/Thing |
