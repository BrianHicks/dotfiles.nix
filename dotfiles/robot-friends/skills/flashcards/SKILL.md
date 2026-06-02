---
name: flashcards
description: Lint a set of flashcards against their source material — find coverage gaps, flag weak cards, and write new cards to fill the holes. Use when the user has source material (their notes and/or the original) plus some already-made flashcards (often just question/answer pairs in a file) and wants to know what's missing or weak. Falls back to generating cards from scratch when no deck exists yet.
---

You are given **source material** and an **existing set of flashcards**. Your
job is to audit the deck against the source: find what the source teaches that
the cards don't yet cover, flag cards that are weak by spaced-repetition
standards, and write **new, atomic** cards that fill the gaps — in the same
shape as the cards you were given. Auditing and writing cards is an act of
understanding, not transcription — interpret the material.

This skill distills three sources; use them as the quality bar:
- Fernando Borretti, *Effective Spaced Repetition*
- Michael Nielsen, *Augmenting Long-term Memory*
- Andy Matuschak, *How to Write Good Prompts*

## Inputs — expect both

- **Source material**: the user's own notes, the original article/paper/book, or
  both. Treat the *notes* as the signal for what the user cares about and how
  they frame it; treat the *original* as ground truth for facts. Either may be a
  path passed as an argument, "this file", or pasted text.
- **Existing flashcards**: most often a plain list of question/answer pairs in a
  file. Could also be a markdown list/table or an Anki export.

If one input is missing, ask for it. If there genuinely are **no existing
cards**, fall back to generating from scratch — same quality bar.

## Reading existing cards

Don't assume one fixed format — **infer the card structure from the document**.
Common shapes:

- A **question line followed by its answer** on the next line(s), pairs often
  separated by a blank line:
  ```
  What is foo bar?
  Baz.
  ```
- A markdown list or table, or `Q:` / `A:` labels, or a heading as the front
  with the body as the back.
- **Cloze** sentences containing `{{c1::…}}` (see syntax below).
- An **Anki export** (tab-separated, `#`-prefixed headers, fields maybe wrapped
  in HTML). It's self-documenting — read the headers and parse accordingly;
  treat HTML wrappers as incidental and judge the card's *content*.

When you write new cards, **mirror the structure you were given** so they drop
straight in — if the input is bare question/answer lines, emit bare
question/answer lines. The only format detail you must actively manage is cloze
syntax. Don't impose Anki headers, decks, or GUIDs unless the input clearly uses
them.

## Process

1. **Gather both inputs.** Ask if one is missing; generate-from-scratch only if
   there are no existing cards.
2. **Understand the source**, then build the **ideal coverage set** — the list
   of atomic, card-worthy facts the material should teach: key terms, core
   claims, techniques, important relationships, and the *why* behind them. Lean
   on the user's notes for what matters most; use the original for accuracy.
   Interpret — include load-bearing insights even if unstated ("cache your
   insight"), after verifying they're correct.
3. **Parse the existing cards** and map each to the fact(s) it covers.
4. **Find gaps** — walk the ideal set; mark each fact covered / partial /
   missing (gap checklist below).
5. **Lint the existing cards** for quality (lint checklist below). Report issues
   and propose fixes, but **don't rewrite the user's cards** unless they ask —
   adding new cards is the default, lower-risk action.
6. **Write new cards** for the gaps: atomic, brief, passing every property, in
   the same shape as the input.
7. **Output** the coverage report, the lint findings, and the new cards.

## Gap checklist — what to hunt for

- **Uncovered fact** — an idea in the source with no card at all.
- **Partial list** — a set where some items are carded and others dropped (e.g.
  a 3-part definition missing one part). Add the missing items in consistent
  order.
- **Missing reverse** — term→definition exists but not definition→term (or vice
  versa).
- **Missing meaning** — a bare fact with no "Why?"/"How?" card to anchor it.
- **Orphan** — an isolated fact not linked to the broader framework; add 1–2
  integrative cards that connect it to what's already there.
- **Single framing** — a key idea phrased only one way; add a variant.
- **Untested nuance** — an important qualifier or condition not captured (e.g.
  "only if adequately detailed", "regardless of how eloquent").
- **Source / attribution** — a named source, author, or date worth a card (only
  if the user cares about it).

## Lint checklist — weak cards to flag (and how to fix)

- **Too broad / not atomic** — combines multiple facts → split it.
- **Vague answer** — open-ended or "…etc." answers (false-negative risk) →
  make the target precise.
- **Clause-level cloze** — a deletion hiding a whole sentence → tighten to the
  load-bearing keyword (one or two words).
- **Yes/no or binary** — a "question smell" → rephrase as open-ended ("Under
  what conditions…?", "What does X mean?").
- **Pattern-matchable** — a distinctive cue gives the answer away mechanically →
  remove the giveaway; keep it short and plain.
- **Ambiguous / not self-contained** — couldn't be answered months later
  without the source → add just enough context, or namespace it ("In sensemaking,
  …").
- **Over-specified** — needless context makes the knowledge feel narrower than
  it is → trim to what disambiguates.
- **Unqualified claim** — a contestable finding stated as fact → reframe as a
  claim ("What does X *claim*…?").

## The quality bar — five properties (every card must pass)

From Matuschak — a good card is:

1. **Focused** — one detail per card; too much detail dulls recall.
2. **Precise** — vague questions get vague answers; say exactly what you want.
3. **Consistent** — elicits the *same* answer every time (inconsistency causes
   interference and erodes related memories).
4. **Tractable** — answerable ~90% of the time; if too hard, break it down or
   add a cue.
5. **Effortful** — requires genuine retrieval; the answer can't be trivially
   inferred from the wording.

## Card-craft rules (the standard you audit and write to)

- **Atomic & brief — the headline rule.** One fact per card; refer to as little
  as possible. When in doubt, split. Write *more* cards than feels natural.
- **Never put a whole list in one card.** One cloze per item; consistent order
  so you learn the set's "shape". For sequences add adjacency ("what comes after
  X?") and position cards.
- **Open-ended categories** — don't ask for "the complete list." Use
  instance→category, pattern-recognition, and generative ("name an example")
  cards instead.
- **Both directions, multiple framings** — term→definition *and*
  definition→term; formal *and* informal phrasings.
- **Add meaning** — pair facts with "Why?"/"How?" cards. For concepts probe
  several lenses: attributes, similarities/differences vs. adjacent ideas,
  parts/wholes, causes/effects, significance, and what it is *not*.
- **No orphans** — connect new facts into a nucleus of related cards.
- **Redundancy across the deck is fine** — memory is frequency × volume; each
  card brief, the deck as repetitive as you like.
- **Cloze vs. Q&A** — cloze when the fact lives naturally inside a sentence
  (terminology in context, a value in a memorable statement, one list item);
  Q&A for definitions, why/how, relationships, procedures. Many facts deserve
  both.

## Cloze syntax

Fill-in-the-blank cards use Anki's cloze notation, which you should produce
directly:

- `{{c1::answer}}` hides "answer"; `{{c1::answer::hint}}` shows a hint in the
  blank.
- Different numbers → **separate** cards from one sentence
  (`The {{c1::mitochondria}} is the {{c2::powerhouse}} of the cell.` → 2 cards).
- The **same** number → revealed **together** on one card (for tightly-coupled
  pieces).
- Keep each deletion to one or two words. Put arbitrary mnemonics in parentheses
  in the answer for elaborative encoding.

## Output

1. **Coverage report** — lead with what matters. A compact list/table:
   - each key fact → **covered / partial / missing**, and
   - each lint finding → which existing card, the problem, the suggested fix.
2. **New cards** — the gap-filling cards **only**, in the **same shape as the
   input** (bare question/answer pairs if that's what you got; cloze sentences
   where fill-in-the-blank fits). Show them inline, and write them to a file
   next to the input (e.g. `<name>-additions.md`) so they're easy to merge.
3. **Wrap up** — report the counts (facts checked, gaps filled, weak cards
   flagged) and offer to apply the proposed rewrites to existing cards if the
   user wants them.

## Worked example (lint + gap-fill)

Source note: *"The ideal response: decide without being emotionally affected,
without FOMO, and without panic."* Existing card (bare Q/A pairs):

```
What is the ideal response to accelerating change?
Deciding without being emotionally affected and panicking.
```

Findings:
- **Partial list** — folds two of three qualities together and **drops "FOMO"**.
- **Not atomic** — three qualities in one prompt; recall will be incomplete.

Fix = add atomic cards in the same bare Q/A shape, leaving the original in place:

```
The ideal response lets you decide without being what (1 of 3)?
Emotionally affected.

The ideal response lets you decide without what feeling (2 of 3)?
FOMO.

The ideal response lets you decide without what (3 of 3)?
Distraction and panic.
```

Or, where a sentence carries the idea better, as cloze:

```
The ideal response is deciding without being {{c1::emotionally affected}}, without {{c2::FOMO}}, and without {{c3::distraction and panic}}.
```
