# DDD Discovery Session: [Domain Name]
**Date:** [Date]
**Participants:** [Names]
**Business Outcome:** [One sentence description]

---

## PHASE 1: Happy Path Event Timeline

### The Story
[SME describes the process in their words]

### Domain Events (Chronological)
```mermaid
graph LR
    A[TriggerEvent] --> B[Event1]
    B --> C[Event2]
    C --> D[Event3]
```

| Event Name | When it happens | Who/What triggered it |
|------------|----------------|----------------------|
|            |                |                      |

---

## PHASE 2: Event Deep Dive

### [EventName]
- **Command:** 
- **Actor:** 
- **Business Rules:**
  - 
- **Data Required:**
  - 
- **Downstream Events:**
  - 
- **Alternative Flows:**
  - 

---

## PHASE 3: Aggregates

### [AggregateName]
**Responsibility:** [What does this protect/enforce?]

**Commands Handled:**
- 
**Events Emitted:**
- 
**Invariants:**
- 
**State:**
- 

---

## PHASE 4: Supporting Patterns

### Read Models
| Name | Purpose | Data Sources |
|------|---------|-------------|

### Policies (Event → Command)
| When (Event) | Then (Command) | Why |
|--------------|----------------|-----|

### External Systems
| System | Integration Point | Events In/Out |
|--------|------------------|---------------|

---

## Bounded Context
```mermaid
graph TB
    subgraph "Core Domain"
        A[Aggregate1]
        B[Aggregate2]
    end
    subgraph "Supporting Domain"
        C[Aggregate3]
    end
    A -->|Event| C
```

---

## Ubiquitous Language

| Term | Definition | Used By |
|------|------------|---------|
|      |            |         |

---

## Open Questions / Parking Lot
-