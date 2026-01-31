# DDD Pattern Recognition Cheat Sheet

## Event Storming Colors (Mental Model)
- **Orange** = Domain Events (OrderPlaced, PaymentReceived)
- **Blue** = Commands (PlaceOrder, ProcessPayment)
- **Yellow** = Aggregates/Entities (Order, Payment)
- **Pink** = Policies (When OrderPlaced → SendConfirmationEmail)
- **Purple** = External Systems
- **Green** = Read Models/Views
- **Red** = Hotspots/Questions

## Aggregate Smell Tests
✅ Good aggregate candidate:
- Has a clear lifecycle
- Enforces business invariants
- Changes atomically
- Has a clear identity

❌ Not an aggregate:
- Just holds data (anemic)
- No behavior/rules
- Frequently joins with others
- No consistency boundary

## Event vs Command
- **Event:** Past tense, factual, immutable (CustomerRegistered)
- **Command:** Imperative, intent, can fail (RegisterCustomer)

## Finding Bounded Contexts
Look for:
- Different meanings of same word (Customer in Sales vs Support)
- Different teams/departments
- Different change rates
- Privacy/security boundaries

Useful probes:
- “Does this word mean the same thing for everyone?”
- “Who owns this decision?” (team/role)
- “What must be consistent immediately vs eventually?”
- “Where do we accept delay/async processing?”

---

## Fast Prompts for Better Signal
- “Give me a concrete example with real values.”
- “What would make this command fail?”
- “Who is allowed to do this, and why?”
- “What must never be true?” (invariant discovery)
- “If this happens twice, what should happen?” (idempotency/duplicates)
- “What happens if this arrives out of order?” (temporal logic)

## Outputs to Make Explicit (Don’t Leave Implicit)
- Architecture Signals: consistency boundaries, storage implications, coupling risks
- Session ADRs: the key decisions made, options considered, and reasons

---

## How to Run the Session

### Pre-Session (5 min)
1. Open your LLM chat/tool of choice
2. Start message:
```
   We're starting a DDD discovery session for [DOMAIN NAME].
   The business outcome we're modeling is: [ONE SENTENCE].
   
   Begin Phase 1: Happy Path Exploration.
```

### During Session (60-90 min)
- **Let the facilitator drive the questions** based on the protocol
- SME answers naturally
- The facilitator builds artifacts progressively
- You can interject: "I think we have an aggregate here"
- Request visualizations: "Show the timeline as a Mermaid diagram"

### Key Commands You Can Give the Facilitator Mid-Session:
```
"Let's move to Phase 2, focus on the [EventName] event"
"I think this is an aggregate - validate it"
"Show me the current aggregate map"
"Capture that phrase in the ubiquitous language glossary"
"Let's park that question and come back to it"
"Generate the event timeline so far"
"Keep a single consolidated Artifacts section updated each turn"
"Summarize uncertainties as Open Questions / Parking Lot"
"List the business rules as Given/When/Then examples"
```

### Post-Session (10 min)
```
"Generate a complete summary with all artifacts"
"Export this as markdown I can save to our repo"
"What are the top 3 architectural risks you see?"
"What assumptions did we make that need validation?"
```
