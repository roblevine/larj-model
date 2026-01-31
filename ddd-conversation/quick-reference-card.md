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
```

---

## How to Run the Session

### Pre-Session (5 min)
1. Open Claude, select your "DDD Discovery Sessions" project
2. Start message:
```
   We're starting a DDD discovery session for [DOMAIN NAME].
   The business outcome we're modeling is: [ONE SENTENCE].
   
   Begin Phase 1: Happy Path Exploration.
```

### During Session (60-90 min)
- **Let Claude drive the questions** based on the protocol
- SME answers naturally
- Claude builds artifacts progressively
- You can interject: "Claude, I think we have an aggregate here"
- Request visualizations: "Show the timeline as a Mermaid diagram"

### Key Commands You Can Give Claude Mid-Session:
```
"Let's move to Phase 2, focus on the [EventName] event"
"I think this is an aggregate - validate it"
"Show me the current aggregate map"
"Capture that phrase in the ubiquitous language glossary"
"Let's park that question and come back to it"
"Generate the event timeline so far"
```

### Post-Session (10 min)
```
"Generate a complete summary with all artifacts"
"Export this as markdown I can save to our repo"
"What are the top 3 architectural risks you see?"