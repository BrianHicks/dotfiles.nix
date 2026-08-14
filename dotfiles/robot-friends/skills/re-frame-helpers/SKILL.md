---
name: re-frame-helpers
description: How to use the defevent, defsub, and defxhrio macros from the re-frame-helpers ClojureScript library — macro expansions, sub-injection with `:<-`, the xhrio request/loading/error pattern, and the gotchas. Use when reading or writing ClojureScript that calls defevent, defsub, or defxhrio, or when something about `:subs` coeffects, `:xhrio/*` effects, or `!xhrio-defaults` needs explaining.
---

# re-frame-helpers

`defevent`, `defsub`, and `defxhrio` come from **re-frame-helpers**, an external
dependency — they will not be defined anywhere in the project source. Do not go looking
for them in `src/`.

- Source: <https://github.com/adrielsantiago/re-frame-helpers>
- All three are macros in `re-frame-helpers.macros` (a `.cljc` file)

Require them with `:refer-macros`, not `:refer`:

```clojure
(ns my.ns
  (:require [re-frame-helpers.macros :refer-macros [defevent defsub defxhrio]]))
```

The library is small (~200 lines across `macros.cljc`, `interceptors.cljs`, `xhrio.cljs`,
`utils.cljs`). If you need to read it, prefer the jar already in the local maven cache over
fetching from GitHub — find the exact coordinate in the project's `shadow-cljs.edn` or
`project.clj`, then:

```bash
unzip -o -d /tmp/rfh ~/.m2/repository/<group>/re-frame-helpers/<version>/re-frame-helpers-<version>.jar
```

## The one thing all three share: the DEBUG export

Every one of these macros, in addition to registering with re-frame, emits a
`(def <name> <handler-fn>)` guarded by `^boolean goog/DEBUG`. The var name is the event/sub
id **with its namespace dropped**:

```clojure
(defevent :my/event-occurred (fn [] {}))
;; also, in dev builds only:  (def event-occurred (fn [] {}))
```

This exists so tests can import and call the raw handler directly, with no re-frame
registry or app-db involved:

```clojure
(ns my.events-test
  (:require [my.events :refer [event-occurred]]))

(deftest handles-it
  (is (= {:db {:a 1}} (event-occurred {:db {}} [:my/event-occurred 1]))))
```

Consequences worth remembering:

- The var only exists when `goog/DEBUG` is true — dev and test builds, not `:advanced`
  release builds. Never reference it from production code.
- Two ids in the same namespace that differ only by their re-frame namespace
  (`:a/save` and `:b/save`) both try to `def save`. Avoid that collision.
- `defxhrio` expands to both a `defsub` and a `defevent` under the same id, so it defs the
  symbol twice; the `defevent` handler wins. Don't rely on the exported var for
  `defxhrio` ids.

## `defsub`

A thin wrapper over `re-frame.core/reg-sub` — every argument is passed straight through,
so the full `reg-sub` syntax works unchanged, including the `:<-` signal sugar and the
3-arity signal-fn form.

```clojure
(defsub :ns/items (fn [db _] (:items db)))

(defsub :ns/item-count
  :<- [:ns/items]
  (fn [items _] (count items)))
```

The only difference from `reg-sub` is the DEBUG export described above, which is taken from
the **last** argument (the computation fn).

## `defevent`

Always expands to `re-frame.core/reg-event-fx` — never `reg-event-db`. Handlers must
return an **effects map** (`{:db ...}`, `{:fx ...}`, `{:dispatch-n ...}`), never a bare db
value.

Argument order is strict:

```
(defevent <id> [<interceptors>]? (:<- [<query-v>])* <handler>)
```

- The interceptor vector, if present, must be **first**.
- Any `:<-` sub-injection pairs come **after** the interceptors.
- The handler is **last**.

### Sub injection with `:<-`

Dereferencing a subscription inside an event handler risks a memory leak (an undisposed
Reagent reaction). `:<-` injects sub values into the coeffects instead. They arrive as a
vector under the `:subs` key, in declaration order:

```clojure
(defevent
  :ns/one-sub
  :<- [:ns/current-user]
  (fn [{[user] :subs}]
    {:fx [[:dispatch [:ns/greet user]]]}))

(defevent
  :ns/several-subs
  [some-interceptor]
  :<- [:sub-a]
  :<- [:sub-b]
  (fn [{:keys [db] [a b] :subs}]
    {:db (assoc db :a a :b b)}))
```

The injecting interceptor looks the value up in re-frame's sub cache first; on a miss it
computes the value, skips caching it, and disposes the reaction immediately. It also
removes `:subs` from the coeffects in its `:after` stage.

Malformed `:<-` pairs do **not** fail compilation — the macro emits a `console.error` at
registration time in DEBUG builds and silently registers with no injection otherwise. If
injected subs are mysteriously absent, check the browser console and re-check argument
order.

## `defxhrio`

The workhorse for HTTP. One id gives you the dispatch, the response, the loading flag, and
the error. It expands to three things:

1. `(defsub <id> #(get % <id>))` — subscribe to the same id to read request state
2. `(defevent <id> [assoc-response xhrio <your-interceptors>...] <handler>)`
3. `(rf/dispatch [:xhrio/init-key <id>])` at namespace load, seeding
   `{:response nil :loading? false :error nil}` into the db under `<id>`

`:<-` sub injection works here too, same as `defevent`.

### The pattern

```clojure
(defxhrio
  :ns/fetch-items
  (fn []
    {:http-xhrio {:uri "/api/items/"}
     :xhrio/dispatch-n-on-success [[:ns/good-fetch-items]]}))

(defevent
  :ns/good-fetch-items
  :<- [:ns/fetch-items]           ;; <- read the response back out
  (fn [{[{:keys [response]}] :subs}]
    {:fx [[:dispatch [:ns/items response]]]}))
```

That pairing — a `defxhrio` plus a `defevent` that injects the same id to get at the
response — is the dominant idiom wherever this library is used.

In a component, dispatch and subscribe with the same key:

```clojure
(let [{:keys [response loading? error]} @(rf/subscribe [:ns/fetch-items])]
  [:button {:on-click #(rf/dispatch [:ns/fetch-items])} "Load"]
  (cond loading? [spinner] error [error-msg error] response [table response]))
```

The db entry under the id transitions:
`{:response nil :loading? false :error nil}` → `{... :loading? true ...}` on dispatch →
`{:response <data> :loading? false :error nil}` or `{:response nil :loading? false :error <err>}`.

### Extra effects

These are returned as **siblings of `:http-xhrio`**, not inside it:

| Effect | Expects | Does |
|---|---|---|
| `:xhrio/dispatch-n-on-success` | vector of event vectors | dispatched after a successful request |
| `:xhrio/dispatch-n-on-failure` | vector of event vectors | dispatched after a failed request |
| `:xhrio/mutate-response` | fn of the response | its return value is what gets stored |
| `:xhrio/mutate-error` | fn of the error | its return value is what gets stored |

**Gotcha:** everything cljs-ajax cares about (`:method`, `:uri`, `:params`, `:format`,
`:response-format`, `:headers`, `:on-success`) goes **inside** `:http-xhrio`. Only the
`:xhrio/*` keys above are siblings. Putting `:response-format` at the top level compiles
fine and is silently ignored — the request just uses the default format. This is an easy
mistake to propagate, so check a neighboring block before copying it.

### Defaults

The `xhrio` interceptor merges, lowest precedence first:

1. `{:method :get, :format (ajax/json-request-format), :response-format (ajax/json-response-format {:keywords? true})}`
2. `@re-frame-helpers.xhrio/!xhrio-defaults` — values may be either a plain value or a fn
   of the coeffects, called at request time
3. whatever your handler put in `:http-xhrio` (wins)

`!xhrio-defaults` is an atom; set it with `reset!`, not `set!` (the upstream README example
is wrong about this). Set it once during app startup:

```clojure
(reset! !xhrio-defaults {:with-credentials false :headers headers})
```

`:on-success` / `:on-failure` default to the internal `:xhrio/good-request` /
`:xhrio/bad-request` handlers that write the response/loading/error map. Supplying your own
overrides them — and thereby opts out of the whole state-tracking pattern. Prefer
`:xhrio/dispatch-n-on-success` when you just want an extra event to fire.

The `assoc-response` interceptor sets the effects `:db` from the coeffects db merged over
any `:db` your handler returned, then writes the loading state. Returning `:db` from a
`defxhrio` handler works, but the request-state key is always applied last.

A project may override `:response-format` per request with its own formatter (e.g. one that
transforms key casing). When a response's keys don't look like you expect, check whether
that event overrides `:response-format` inside `:http-xhrio` — the library default does not
transform key casing.
