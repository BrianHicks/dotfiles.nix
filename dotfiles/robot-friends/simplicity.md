# Design (every language)

- **Apply the four rules of simple design, in order** (Beck). The code passes the tests, reveals intent, duplicates no knowledge, then has the fewest elements. Brevity comes last. Never trade clarity for a shorter diff.
- **Make the change easy, then make the easy change** (Beck). When the code fights a change, refactor first as its own step.
- **Tidy first, separately** (Beck, *Tidy First?*). Put structural and behavior changes in different commits. Say where the split goes so I can make it.
- **Choose clear over clever** (Pike). Prefer plain control flow and obvious data to tricks, metaprogramming or dense one-liners.
- **Design the data first** (Pike). Get the data structures and boundaries right, and the code follows. Keep typed objects inside and convert at the edge.
- **Choose simple over easy** (Hickey). Pick the option with fewer intertwined parts, not the familiar one. Keep state, time, logic and I/O separate.
- **Keep a functional core inside an imperative shell** (Bernhardt). Make decisions in pure functions over plain data. Put queries, HTTP and tasks at the edges.
- **Lean on types, not classes** (Minsky). Behavior is plain functions under a named type. Data is immutable records. Don't write single-method interfaces or classes that only wrap a function. Don't pass names as strings where a typed field would let the checker catch the mistake.
- **Build deep modules** (Ousterhout). Put a simple interface over real work. Don't split a function or service just to make it shorter.
- **Define errors out of existence** (Ousterhout). Prefer an API where the bad case can't happen over handling it at every call site.
- **Give each action its own route, never a sentinel value** (Fowler). An action like reset gets its own route or event. It never rides as a nil or empty payload on an existing one.
- **Reuse before adding** (Hunt & Thomas). Before a new flag, helper, setting or export, search the repo for one that already does the job. Name it in the plan.
- **Copy until the third time, but never copy a business rule** (Roberts, via Fowler). Duplication is cheaper than the wrong abstraction (Metz). A little copying beats a new dependency (Pike). When an abstraction stops fitting, inline it back rather than adding parameters.
- **Keep interfaces small** (Pike). Give each few parameters and one purpose. Add no interface, factory or config without a second real user.
- **Measure before optimizing** (Pike). Add no speculative caching or cleverness without a profile, query count or trace.
- **Fix at the root, once** (Ohno). Change the shared function every caller uses, not each call site.
- **Fix data at its source** (Ohno). Fix the writer going forward. Never write a migration that guesses a value for records that never had it, and keep the error signal for legacy rows. Before filtering bad rows out of a chart, check whether they come from an already-fixed bug that will age out.
- **Characterize before changing** (Feathers). Give untested code a test of its current behavior before the edit.

## Deciding (Parrish, *The Great Mental Models*)

- **Run every design, plan and recommendation through the models below.** Name in the plan the ones that changed the answer. Don't list the ones that didn't.

### Scoping

- **Essentialism** (McKeown): treat each step as a clear yes or a no. Every plan names what it won't do.
- **The ONE Thing** (Keller): name the step that makes the others easier or unnecessary, and do it first.
- **80/20** (Koch): rank the parts by value. Propose cutting the low-value majority.

### Planning and reviewing

- **Inversion** (Jacobi, via Munger): ask how a plan or a change under review could fail or make things worse. Look for data loss, an outage or silently wrong output. Design against that first.
- **Second-order thinking** (Marks): follow each option one step further. Check other callers, load, migrations, on-call and the next change someone will make here.
- **First principles** (Aristotle): separate the real constraint from inherited convention or existing code. When the convention is the problem, rebuild from the constraint.
- **Hanlon's razor** (Hanlon): in reviews and incidents, assume an honest mistake or a missing constraint, not carelessness.

### Debugging

- **Occam's razor** (Ockham): test the simplest explanation that fits all the evidence first.
- **The map is not the territory** (Korzybski): treat docs, specs, dashboards and memory of the code as maps. Confirm against the code, a test or real telemetry before acting on them.

### Removing anything

- **Chesterton's fence** (Chesterton): before removing a guard, rule or odd-looking code, find out why someone added it (`jj log`, the MR, the issue).

### Sizing limits and rollouts

- **Margin of safety** (Graham): size timeouts, limits, capacity and rollouts with headroom beyond the expected case.

### Uncertainty

- **Circle of competence** (Buffett and Munger): say when a decision rests on a library, domain or system you haven't verified. Then verify it or flag it. Don't guess.
- **Probabilistic thinking** (Duke, *Thinking in Bets*): weigh options by likelihood times the cost of being wrong. Prefer reversible moves with a small blast radius. Spend the most care on rare but catastrophic paths. State confidence as a rough percentage on hypotheses, verdicts and recommendations.
- **Measure to decide, then stop** (Duke, *Quit*): run one A/B per change, for a few rounds. Once the results sit inside the noise of code neither side touched, call it. Don't rerun to chase single groups at the edge of the noise. Set a budget and kill criteria up front, and stop when one hits.
