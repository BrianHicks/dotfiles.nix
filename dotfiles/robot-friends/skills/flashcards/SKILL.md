---
name: flashcards
description: Turn a document into atomic Anki flashcards — question/answer pairs and cloze deletions following spaced-repetition best practices. Use when the user wants to make flashcards, Anki cards, cloze deletions, or study prompts from notes, an article, a paper, or any document.
---

Read a document and produce a set of **atomic, brief** spaced-repetition
flashcards in Anki's import syntax. Writing good cards is an act of
understanding, not transcription — interpret the material, then encode it.

This skill distills three sources; follow all of them:
- Fernando Borretti, *Effective Spaced Repetition*
- Michael Nielsen, *Augmenting Long-term Memory*
- Andy Matuschak, *How to Write Good Prompts*

## Process

1. **Get the document.** If a path or text was passed as an argument, use it.
   If the user pointed at "this file" or the current buffer, read that. If you
   have nothing concrete, ask which document before continuing.
2. **Understand it first.** Do not ankify material you don't understand —
   you'd just be memorizing noise. Clarify the concepts until you have a solid
   grasp. If reasoning reveals an unstated insight, capture it too ("cache your
   insight"), but verify it's correct first.
3. **Be selective.** Don't ankify everything. Extract the load-bearing ideas:
   key terms, core claims, central techniques, important relationships, and the
   *why* behind them. For a quick pass aim for ~5–20 cards; go deeper only when
   the material justifies it. Skipping the trivia is a feature.
4. **Decompose into atomic facts.** Break each idea into the smallest
   standalone pieces. Write *more* cards than feels natural — many tiny cards
   beat one big one.
5. **Write each card**, choosing Q&A or cloze (see below), and run it through
   the checklist.
6. **Check every card** against the five properties and the anti-patterns
   before emitting it. Rewrite or drop any that fail.
7. **Emit** the cards in Anki import format (see Output). Then briefly tell the
   user the count and how to import.

## The five properties (check every card)

From Matuschak — a good card is:

1. **Focused** — one detail per card. Too much detail dulls concentration and
   produces incomplete recall.
2. **Precise** — vague questions get vague answers. Say exactly what you want.
3. **Consistent** — the card should elicit the *same* answer every time.
   Inconsistent prompts cause interference and erode related memories.
4. **Tractable** — you should be able to answer it ~90% of the time. If it's
   too hard, break it down or add a cue.
5. **Effortful** — the answer must require genuine retrieval; you shouldn't be
   able to trivially infer it from the question's wording.

## Core rules

**Atomicity & brevity — the headline rule.**
Cards should be short and refer to as little information as possible. One fact
per card. Larger cards are harder to recall and create grading ambiguity (right
on part, wrong on part). When in doubt, split.

**Don't memorize a list in one card.**
Break enumerations into individual cards — typically one cloze per item. Keep
the items in a *consistent order* so you also learn the "shape" of the set. For
ordered sequences, additionally make adjacency cards ("what comes after X?",
"what comes before X?") and position cards.

**Open-ended categories: don't ask for "the complete list."**
For sets with no fixed membership, write instance→category cards ("X is an
example of what?"), pattern-recognition cards ("what do these instances share?"),
and generative cards ("name an example of Y" — answered from memory or fresh).

**Ask in both directions and multiple ways.**
For a term and its definition, make both term→definition and
definition→term. Ask for formal *and* informal phrasings. Multiple framings of
one idea build richer associative paths and reduce pattern-matching.

**Add meaning, not just facts.**
Pair raw facts with "Why?" / "How?" / explanation cards. For concepts, probe
multiple lenses: attributes, similarities and differences vs. adjacent ideas,
parts/wholes, causes/effects, significance, and what the concept is *not*.

**Avoid orphans — build a nucleus.**
Never leave a single isolated card on a tangential topic; it decays fast. Add
2–3 connected cards and some integrative cards that link the new fact to things
already known. Build from simple facts up to higher-level connections.

**Redundancy across the deck is good.**
Memory is frequency × volume. Each *card* stays brief, but the *deck* can repeat
a concept from many angles freely. Don't feel guilty about overlap.

## Anti-patterns — reject cards that do these

- **No yes/no or binary questions.** "Is X true?" is a smell. Rephrase as
  open-ended: "Under what conditions is X true?", "What does X mean?"
- **No pattern-matching shortcuts.** Don't include unusual/distinctive words or
  giveaway cues that let you answer mechanically without recalling the fact.
  Keep questions short and simple.
- **No ambiguity (false negatives).** The card must be self-contained. Test:
  *could I answer this months from now without the source?* If a reasonable
  alternative answer exists, add just enough context to rule it out.
- **Namespace when domains collide.** If you're learning similar things in two
  contexts, name the context: "In Emacs, how do you delete a word?" not a bare
  "how do you delete a word?".
- **Don't over-specify.** Adding needless context can make knowledge feel more
  narrow/provincial than it is. Include enough to disambiguate, no more.
- **Qualify claims.** Frame contestable findings as claims, not facts: "What
  does Jones 2011 *claim* the average age is?" rather than stating it outright.
- **Don't ankify what you don't understand**, and don't transcribe verbatim —
  interpret.

## Q&A vs. cloze — which to use

Use **cloze deletion** when the fact lives naturally inside a sentence or phrase:
terminology in context, a value or name within a memorable statement, or one
item of a list. Cloze keeps surrounding context visible, which aids recall.

Use **Q&A (Basic)** when you want a direct prompt→response: definitions (both
directions), why/how questions, relationships, and procedures.

Many facts are worth encoding *both* ways across the deck. Prefer whichever
makes the retrieval most focused and effortful.

## Anki syntax reference

**Cloze deletion** (Cloze note type, one `Text` field):
- `{{c1::hidden answer}}` — hides "hidden answer".
- `{{c1::answer::hint}}` — shows "hint" in place of the blank.
- Different numbers make **separate** cards from one note:
  `The {{c1::mitochondria}} is the {{c2::powerhouse}} of the cell.` → 2 cards.
- The **same** number reveals together: `{{c1::a}}` and `{{c1::b}}` hide on the
  same card. Use this to blank tightly-coupled pieces at once.
- Keep each deletion small — one or two words is ideal (atomicity applies here
  too). Avoid blanking so much of the sentence that it's unanswerable.
- Put arbitrary mnemonics or vivid associations in parentheses in the answer to
  aid elaborative encoding.

**Q&A** (Basic note type): a `Front` and a `Back` field.

## Output format

Emit cards as Anki text-import blocks with header lines so they import cleanly.
Use **tab** as the field separator. Keep each note on a single line (use `<br>`
for an intentional line break and set `#html:true` if you do).

Produce up to two fenced code blocks — one per note type used — and also write
each to a file in the working directory (`flashcards-basic.txt`,
`flashcards-cloze.txt`) so the user can import directly.

**Basic (Q&A):**
```
#separator:tab
#html:false
#notetype:Basic
#deck:Default
What command creates a soft link?	ln -s
In `ln -s`, which path comes first — target or link name?	the target (the existing file)
```

**Cloze:**
```
#separator:tab
#html:false
#notetype:Cloze
#deck:Default
A soft link is created with {{c1::ln -s}}.
The {{c1::target}} path is given before the {{c2::link name}} in `ln -s`.
```

Tell the user: in Anki, **File → Import**, pick the file; the headers set the
note type, deck, and separator automatically. Mention how many cards you made
and offer to revise any (watch for cards that make you mentally "sigh").

## Worked example

Source sentence: *"In 1856, working with pea plants, Mendel discovered that
traits are inherited as discrete units, later called genes."*

Too big (reject): `Front: Tell me about Mendel's 1856 discovery / Back: …`.

Atomic cards instead:
- Basic: `Who discovered that traits are inherited as discrete units?` → `Mendel`
- Basic: `What organism did Mendel use in his inheritance experiments?` → `pea plants`
- Basic: `In what year did Mendel begin his inheritance work?` → `1856`
- Basic (reverse): `What did Mendel call the discrete units of inheritance (later named genes)?` → `discrete units / "factors"`
- Cloze: `Mendel worked with {{c1::pea plants}} to study inheritance.`
- Cloze: `Mendel showed traits are inherited as {{c1::discrete units}}, later called {{c2::genes}}.`
- Meaning (Basic): `Why was Mendel's "discrete units" idea significant?` → `it contradicted blending inheritance — traits don't average, they pass on intact`
